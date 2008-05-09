(* $Id$
 *
 * 20070619 T. Bourke
 *   Adapted from `Modern compiler implementation in ML', Appel 1998.
 *
 *)


(* TODO:
    * clean up the storage of scope, making it more consistent.
 *)

structure Environment : ENVIRONMENT
(*structure Environment :> ENVIRONMENT  (* TODO: sort out mlton & sml/nj *)
                             where (Expression = Expression)
                             where (Declaration = Declaration)*) =
struct
  structure Expression = Expression
  structure E = Expression
  structure Declaration = Declaration
  structure D = Declaration

  structure ECVT = ExpressionCvt

  exception UndeclaredTypeName of string
  exception CannotStaticallyEvaluate of string
  exception DuplicateDefinition of string
  exception VariableWithoutType of string

  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:= ; open Symbol

  type ty = Expression.ty
  type expr = Expression.expr
  type decl = Declaration.decl
  type stmt = Declaration.stmt
  type symbol = Atom.atom

  structure Map = AtomMap
  type 'a table = (int * 'a) Map.map

  (* Track scope so as to identify shared variables. *)
  datatype scopetag = GlobalScope       (* defined globally *)
                    | ParameterScope    (* template parameter *)
                    | TemplateScope     (* defined locally to template *)
                    | LocalScope        (* function parameter or local *)
                    | SelectScope       (* select id on transition *)
                    | BoundScope        (* bound variable (forall, exists) *)

  datatype enventry = VarEntry of {ty: ty, init: D.initialiser option,
                                   ref: bool, scope: scopetag}
                    | FunEntry of {formals: {ty:ty, id:symbol, ref: bool} list,
                                   result: ty, body: stmt, scope : scopetag}

  type tyentry = scopetag * ty

  type env = tyentry table * enventry table * int
    (* Each entry, in either table, is stamped with an integer, indicating
     * when it was inserted. This allows a simple approximation of
     * dependencies (provided insertions are performed in that order).
     * The int tracks the next stamp. *)

  val base_tenv = Map.empty
  val base_venv = Map.empty
  val base_env  = (base_tenv, base_venv, 0)

  (* NB: the base types are parsed in directly which simplifies handling
         bounded integer expressions (e.g. int[-5,5]) *)
  val base_tenv = Map.empty

  (* NB: there are no variables to begin with *)
  val base_venv = Map.empty

  fun isEmpty (tenv, venv, _) = Map.isEmpty tenv andalso Map.isEmpty venv

  (* SimpleInit, ArrayInit *)
  fun compileTimeEval ((tenv, venv, _), e) = e
(*  let
      fun proc (VarExpr v)          =  ... getvalue, check const ...
        | proc (v as (IntCExpr _))  = v
        | proc (v as (BoolCExpr _)) = v
        | proc (v as (CallExpr
              (* Need an interpreter for funtions!... *)
    in
      proc e
    end
*)

  local fun resolve (E.Const, _)    = E.Const (* Const is strongest *)
          | resolve (_, E.Const)    = E.Const
          | resolve (E.NoQual, tq') = tq'   (* NoQual is weakest  *)
          | resolve (tq, _)         = tq
  in
  fun applyTyQual (E.NoQual, t) = t
    | applyTyQual (tq, E.INT (b, tq'))      = E.INT (b, resolve (tq, tq'))
    | applyTyQual (tq, E.BOOL tq')          = E.BOOL (resolve (tq, tq'))
    | applyTyQual (_, E.CLOCK)              = E.CLOCK
    | applyTyQual (_, t as (E.CHANNEL _))   = t
    | applyTyQual (tq, E.SCALAR (e, tq', u))= E.SCALAR (e,
                                                resolve (tq, tq'), u)
    | applyTyQual (tq, E.RECORD (fields, tq', u)) = E.RECORD (
       map (fn (s, t)=> (s, applyTyQual (tq, t))) fields, resolve (tq, tq'), u)
    | applyTyQual (tq, E.ARRAY (ty, e))     = E.ARRAY (applyTyQual (tq, ty), e)
    | applyTyQual (tq, E.NAME (s, tq', NONE)) =
                                            E.NAME (s, resolve (tq, tq'), NONE)
    | applyTyQual (tq, E.NAME (s, tq', SOME exty)) =
                  E.NAME (s, resolve (tq, tq'), SOME (applyTyQual (tq, exty)))
    | applyTyQual (tq, E.VOID)              = E.VOID
  end

  (* Expand out all E.NAME entries from the given type.
     Reduce all expressions as far as possible. *)
  fun expandTyIds (env as (tenv, venv, _), t) = let
    fun expandField (s, t) = (s, expand t)
    and expand (E.INT (NONE, tq))          = E.INT (NONE, tq)
      | expand (E.INT (SOME (e1, e2), tq)) = E.INT (SOME (
                                              compileTimeEval (env, e1),
                                              compileTimeEval (env, e2)), tq)
      | expand (E.SCALAR (e, tq, u))       = E.SCALAR (
                                              compileTimeEval (env, e), tq, u)
      | expand (E.RECORD (fields, tyqual, u))
                                      = E.RECORD (map expandField fields,
                                                  tyqual, u)
      | expand (E.ARRAY (ty, E.Type sty))  = E.ARRAY (expand ty,
                                                      E.Type (expand sty))
      | expand (E.ARRAY (ty, E.Unresolved s)) = let
              val sty = case Map.find (tenv, s)
                        of SOME (_, (_, ety)) => ety
                         | NONE          => let
                             val u = E.VarExpr (E.SimpleVar s)
                                    (* int[l, u] bounds are inclusive *)
                             val umo = E.dec u
                           in E.INT (SOME (E.IntCExpr 0, umo), E.NoQual) end
            in
              E.ARRAY (expand ty, E.Type (expand sty))
            end

      | expand (E.NAME (s, tyqual, _))   = (case Map.find (tenv, s) of
                     NONE => raise UndeclaredTypeName (Atom.toString s)
                   | SOME (_, (_, ety)) => E.NAME (s, tyqual,
                                             SOME (applyTyQual (tyqual, ety))))
                                     (* assumption: ety is already expanded. *)

      | expand (v as (E.BOOL _))      = v
      | expand (v as (E.CHANNEL _))   = v
      | expand E.CLOCK                = E.CLOCK
      | expand E.VOID                 = E.VOID

    in expand t end


  fun addDeclarations (oenv, scope, decls) = let

    (* (* TODO: sometimes duplicates are ok (e.g. local variable),
                only complain if the scopes are the same? *)
    infix <?; fun  m <? x = isSome (Map.find (m, x))
    
    fun ensureNoDuplicates (d, env as (tenv, venv)) = case D.declToId d
        of NONE => (d, env)
         | SOME id => if tenv <? id orelse venv <? id
                      then raise DuplicateDefinition (Atom.toString id)
                      else (d, env)
    *)
    fun  ensureNoDuplicates (d, env) = (d, env)

    fun addDecl (D.TyDecl {id, ty, ...}, env as (tenv, venv, c)) =
             let
               val e =  expandTyIds (env, ty)

             in
               (Map.insert (tenv, id, (c, (scope, e))), venv, c+1)
             end

      | addDecl (D.VarDecl {id, ty, initial, ...}, env as (tenv, venv, c)) =
            let
              fun evalInit (D.SimpleInit e) = D.SimpleInit
                                                (compileTimeEval (env, e))
                | evalInit (D.ArrayInit l) = D.ArrayInit (map evalInit l)

              val init = case initial of
                           NONE   => NONE
                         | SOME e => SOME (evalInit e)

              val v = VarEntry {ty=expandTyIds (env, ty),
                                init=init, ref=false, scope=scope}

            in (tenv, Map.insert (venv, id, (c, v)), c+1) end

      | addDecl (D.FunDecl {id, rty, params,body, ...}, env as (tenv,venv,c)) =
            let
              fun doParam (D.ValParam {id, ty}) = {ty=expandTyIds (env, ty),
                                                   id=id, ref=false}
                | doParam (D.RefParam {id, ty}) = {ty=expandTyIds (env, ty),
                                                   id=id, ref=true}
              val e = FunEntry {formals=map doParam params,
                                result=expandTyIds (env, rty),
                                body=body, scope=scope}

            in (tenv, Map.insert (venv, id, (c, e)), c+1) end

      | addDecl (D.ChanPriDecl _, env) = env (* ignore *)

    in
      foldl (addDecl o ensureNoDuplicates) oenv decls
    end

  fun addParameters (oenv as (tenv, ovenv, c), scope, params) = let
      fun getVar (r, ty) = VarEntry {ty=expandTyIds (oenv, ty),
                                     init=NONE, ref=r, scope=scope}

      fun doParam (D.ValParam {id, ty}, (c, venv)) =
                        (c+1, Map.insert (venv, id, (c, getVar (false, ty))))
        | doParam (D.RefParam {id, ty}, (c, venv)) =
                        (c+1, Map.insert (venv, id, (c, getVar (true, ty))))

      val (c', venv') = foldl doParam (c, ovenv) params
    in (tenv, venv', c') end

  fun typeEnvToString (tenv, _, _) = Map.foldli (fn (s, (_, (_, t)), pre) =>
                                            pre ^ Atom.toString s ^ ": " ^
                                            ECVT.Ty.toString t ^ "\n") "" tenv

  local
    fun paramToStr {id, ty, ref=false} = Atom.toString id ^
                                         ": " ^ ECVT.Ty.toString ty
      | paramToStr {id, ty, ref=true } = Atom.toString id ^
                                         ": " ^ ECVT.Ty.toString ty ^ "(ref)"

    fun scopeToStr GlobalScope    = "global"
      | scopeToStr ParameterScope = "param"
      | scopeToStr TemplateScope  = "template"
      | scopeToStr LocalScope     = "local"
      | scopeToStr SelectScope    = "select"
      | scopeToStr BoundScope     = "bound"

    fun envEntryToStr (_, VarEntry {ty, scope, ...}) = ECVT.Ty.toString ty ^
                                                   "(" ^ scopeToStr scope ^ ")"
      | envEntryToStr (_, FunEntry {formals, result, body, scope}) =
        (ListFormat.fmt {init="(", sep=",", final=")", fmt=paramToStr} formals)
            ^ " -> " ^ ECVT.Ty.toString result ^ " (" ^ scopeToStr scope ^ ")"
  in
  fun varEnvToString (_, venv, _) = Map.foldli (fn (s, e, pre)=> pre ^
                                                    Atom.toString s ^ ": " ^
                                                    envEntryToStr e ^ "\n") ""
                                                    venv
  end

  fun baseType (E.NAME (_, _, SOME ety)) = ety
    | baseType ty                        = ty

  local
    val <*< = Option.composePartial; infix <*< 

    fun funToType (_, FunEntry {result, ...}) = SOME (baseType result)
      | funToType (_, VarEntry _)             = NONE

    fun varToType (_, VarEntry {ty, ...}) =  SOME (baseType ty)
      | varToType (_, FunEntry _)         =  NONE

    fun stripArray (E.ARRAY (ty, _)) = SOME ty
      | stripArray _                 = NONE
  in
  fun findVarExprType (tenv, venv, _) = let
      fun findField field (E.RECORD (fields, tq, _)) = let
                fun getType (_, t) = SOME (applyTyQual (tq, t))
                fun isField (s, _) = Atom.compare (field, s) = EQUAL
              in
                (getType <*< (List.find isField)) fields
              end
        | findField _ _ = NONE
      
      fun fve (E.SimpleVar s)           = (varToType <*< Map.find) (venv, s)
        | fve (E.ReturnVar {func, ...}) = (funToType <*< Map.find) (venv, func)
        | fve (E.SubscriptVar (v, _))   = (stripArray <*< fve) v
        | fve (E.RecordVar (v, field))  = ((findField field) <*< fve) v
    in fve end

  fun findVal (_, venv, _) s = (Option.compose (fn (_, v)=>v, Map.find)) (venv, s)

  fun findValType (_, venv, _) s = (varToType <*< Map.find) (venv, s)

  fun channels (tenv, venv, _) = let
      fun elemTy (E.ARRAY (ty, _)) = elemTy ty
        | elemTy ty = ty

      fun getChan (id, (_, VarEntry {ty, ...})) = (case elemTy (baseType ty) of
                                                     E.CHANNEL _ => SOME id
                                                   | _           => NONE)
        | getChan _ = NONE

    in Map.listItems (Map.mapPartiali getChan venv) end

  fun usedIds (tenv, venv, _) = let
      fun addId (nm, _, s) = AtomSet.add (s, nm)

      val tids = Map.foldli addId AtomSet.empty tenv
      val vids = Map.foldli addId tids venv
    in vids end
  
  end (* local *)

  local structure SS = Substring in
  fun newId (baseSymbol, (tenv, venv, _)) =
    (*{{{1*)
    if not (Option.isSome (Map.find (venv, baseSymbol))
            orelse Option.isSome (Map.find (tenv, baseSymbol)))
    then baseSymbol
    else let
           fun getPrefix s = SS.string (SS.dropr Char.isDigit (SS.full s))

           fun getSuffix s = let
               val suf = SS.taker Char.isDigit (SS.full s)
             in
               if SS.isEmpty suf then 0
               else valOf (Int.fromString (SS.string suf))
             end
          
           val prefix = getPrefix (Atom.toString baseSymbol)

           fun maxSuffix (a, _, m) = let val s = Atom.toString a in
               if String.isPrefix prefix s
               then Int.max (m, getSuffix s) else m
             end

           val tsuffix = Map.foldli maxSuffix 0 tenv
           val suffix  = (Map.foldli maxSuffix tsuffix venv) + 1
         in
            Atom.atom (prefix ^ Int.toString suffix) 
         end (*}}}1*)
  end (*local*)

  fun addId scope ((id, ty), env as (tenv, venv, c)) = let
      val v = VarEntry {ty=expandTyIds (env, ty), init=NONE,
                        ref=false, scope=scope}
    in (tenv, Map.insert (venv, id, (c, v)), c+1) end

  fun createdAfter ((_, (i, _)), (_, (j, _))) = i > j

  fun writableVariables ((_, venv, _), seekScope) = let
      fun goodTy (E.INT (_, E.NoQual))       = true
        | goodTy (E.BOOL E.NoQual)           = true
        | goodTy (E.SCALAR (_, E.NoQual, _)) = true
        | goodTy (E.RECORD (_, E.NoQual, _)) = true
        | goodTy (E.ARRAY (ty, _))           = goodTy ty
        | goodTy (E.NAME (_, E.NoQual, _))   = true
        | goodTy _                           = false

      fun f (_, (_, FunEntry _), l) = l
        | f (var, (_, entry as VarEntry {ty, scope, ...}), l) =
            if (goodTy ty andalso scope = seekScope) then var::l else l

    in Map.foldli f [] venv end

  fun dupVariables vars (env as (_, ovenv, _), newscope') = let

      fun newscope (v as VarEntry {ty, init, ref=r, scope}) = (case newscope' of
              NONE        => v
            | SOME scope' => VarEntry {ty=ty, init=init, ref=r, scope=scope'})
        | newscope _ = raise Fail "bad call to newscope in dupVariables"

      fun dupVar (_, (_, FunEntry _), s) = s
        | dupVar (var, (_, entry as VarEntry {ty, scope, ...}),
                  s as (oldnewmap, env as (tenv, venv, i))) =
        let
          val var' = newId (var, env)
        in
          (Map.insert (oldnewmap, var, var'),
           (tenv, Map.insert (venv, var', (i, newscope entry)), i+1))
        end

      fun f (v, args) = case Map.find (ovenv, v) of
                          NONE => args
                        | SOME entry => dupVar (v, entry, args)

    in foldl f (Map.empty, env) vars end

  fun mapValues f (_, venv, _) = let
      fun f' (i, (_, d)) = f (i, d)
    in
      List.mapPartial f' (ListMergeSort.sort createdAfter (Map.listItemsi venv))
    end

  fun mapTypes f (tenv, _, _) = let
      fun f' (i, (_, (s, d))) = f (i, s, d)
    in
      List.mapPartial f' (ListMergeSort.sort createdAfter (Map.listItemsi tenv))
    end

  local (*{{{1*)
    datatype mixed = EntryVal of symbol * enventry
                   | EntryTy  of symbol * scopetag * ty

    fun fromVal (i, (c, d))      = (c, EntryVal (i, d))
    fun fromTy  (i, (c, (s, d))) = (c, EntryTy  (i, s, d))
    fun createdAfter' ((i, _), (j, _)) = i > j
  in (*}}}1*)
  fun mapBoth (fVal, fTy) (tenv, venv, _) = let
      fun f' (_, EntryVal d) = fVal d
        | f' (_, EntryTy  d) = fTy d
    in
      List.mapPartial f' (ListMergeSort.sort  createdAfter'
          (List.revAppend (map fromVal (Map.listItemsi venv),
                           map fromTy  (Map.listItemsi tenv))))
    end
  end (* local *)
  
  (* val filter : (env * expr -> bool) -> env -> expr -> expr list *)
  fun filter p env e = let
      fun f (env, e) = if p (env, e) then [e] (*{{{1*)
                else case e
                      of E.VarExpr _   => []
                       | E.IntCExpr _  => [] 
                       | E.BoolCExpr _ => []
                       | E.Deadlock    => []
                       | E.CallExpr {args, ...}    => foldl (flist env) [] args
                       | E.NegExpr expr            => f (env, expr)
                       | E.NotExpr expr            => f (env, expr)
                       | E.UnaryModExpr {expr, ...}=> f (env, expr)
                       | E.BinIntExpr {left, right,...}  => f (env, left)
                                                         @ f (env, right)
                       | E.BinBoolExpr {left, right,...} => f (env, left)
                                                         @ f (env, right)
                       | E.RelExpr {left, right, ...}    => f (env, left)
                                                         @ f (env, right)
                       | E.AssignExpr {var, expr, ...}   => f (env, var)
                                                         @ f (env, expr)
                       | E.CondExpr {test, trueexpr, falseexpr, ...}
                                                         => f (env, test)
                                                         @ f (env, trueexpr)
                                                         @ f (env, falseexpr)

                       | E.ForAllExpr {id, ty, expr, ...} => let
                             val env' = addId BoundScope ((id, ty), env)
                           in f (env', expr) end

                       | E.ExistsExpr {id, ty, expr, ...} => let
                             val env' = addId BoundScope ((id, ty), env)
                           in f (env', expr) end

      and flist env (e, el) = f (env, e) @ el
    in f (env, e) end (*}}}1*)

  local
    fun isClk (env, v) = case findVarExprType env v
                         of SOME (E.CLOCK) => true
                          | NONE           => raise VariableWithoutType
                                                    (E.varName v)
                          | _              => false

    fun isClkVar (env, E.VarExpr v) = isClk (env, v)
      | isClkVar _                  = false
  in
    fun containsClocks env expr =
        not (List.null (filter (fn (env',e)=>isClkVar (env', e)) env expr))
  end
end

