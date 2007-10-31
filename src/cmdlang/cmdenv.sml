(* $Id$
 *
 * 20071024 T. Bourke
 *  Original code.
 *)

structure CmdEnv = struct

  structure Nta = ParsedNta
  datatype direction = datatype Expression.direction

  type symbol = Atom.atom
  val % = Atom.atom
  type symbolset = AtomSet.set

  datatype value = Template   of Nta.template 
                 | Symbols    of AtomSet.set
                 | SymbolMap  of symbol AtomMap.map
                 | Actions    of ActionSet.set
                 | ActionMap  of (symbol * direction) ActionMap.map
                 | Fail       of string
                 | Success

  type t = value AtomMap.map * ParsedNta.nta

  fun listTemplates (m, _) = let
      fun f (Template t, l) = t::l
        | f (_, l)          = l
    in AtomMap.foldl f [] m end

  fun fromNta (nta as Nta.Nta {templates, declaration, ...}) = let
      fun insertTemplate (t as Nta.Template {name=(name, _),...}, m)
                              = AtomMap.insert (m, %name, Template t)

      val envT = foldl insertTemplate AtomMap.empty templates
    in (foldl AtomMap.insert' envT [
         (%"channels", Symbols (
          foldl AtomSet.add' AtomSet.empty (Environment.channels declaration))),

         (%"globals", Symbols (
          foldl AtomSet.add' AtomSet.empty (Environment.writableVariables
                                  (declaration, Environment.GlobalScope))))
        ], nta)
    end

  fun toNta (env as (_, nta)) = let
      val nta' = ParsedNta.Template.mapPartial (fn _=>NONE) nta
    in ParsedNta.renumber (ParsedNta.addTemplates nta' (listTemplates env)) end

  fun insert ((nm, Template t), (m, nta)) = let
          val t' = Nta.Template.updName t (Atom.toString nm)
        in (AtomMap.insert' ((nm, Template t'), m), nta) end

    | insert (p, (m, nta)) = (AtomMap.insert' (p, m), nta)

  fun remove (env as (m, nta), nm) = let
          val (m', v) = AtomMap.remove (m, nm)
                        handle LibBase.NotFound => (m, Fail "not found")
        in ((m', nta), v) end

  fun getValue (m, _) nm = AtomMap.find (m, nm)

  fun listItems (m, _) = AtomMap.listItemsi m

  fun dirToString Input = "?" | dirToString Output = "!"

  fun substToString (nm, nm') = concat [Atom.toString nm, "/",
                                        Atom.toString nm']

  fun actionToString (nm, d) = Atom.toString nm ^
                                 (case d of Input => "?" | Output => "!")

  fun actionsubstToString (from, to) = (actionToString from) ^" -> "^
                                       (actionToString to) 

  fun toString (Template _) = "<template>" (* TODO: improve *)

    | toString (Symbols s)  = concat
            ["{ ", String.concatWith ", "
                     (map Atom.toString (AtomSet.listItems s)), " }"]
    | toString (SymbolMap s) = concat
            ["{ ", String.concatWith ", "
                     (map substToString (AtomMap.listItemsi s)), " }"]

    | toString (Actions s) = ListFormat.fmt {init="{| ", final=" |}", sep=", ",
            fmt=actionToString} (ActionSet.listItems s)
    | toString (ActionMap s) = concat
            ["{| ", String.concatWith ", "
                     (map actionsubstToString (ActionMap.listItemsi s)), " |}"]

    | toString (Fail s) = concat ["<fail: ", s, ">"]
    | toString (Success) = concat ["<ok>"]


  fun globalDecl (_, ParsedNta.Nta {declaration,...}) = declaration

  fun symbolsToActions syms = let
      fun f (s, m) = ActionSet.add (ActionSet.add (m, (s,Input)), (s,Output))
    in AtomSet.foldl f ActionSet.empty syms end

end

