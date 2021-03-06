(* $Id$
 *
 * Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
 * All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the "BSD License" which is distributed with the
 * software in the file LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
 * License for more details.
 *)

structure MakeTimed = struct

  (* shortcuts over Atom and AtomSet *)
  infix <+ <- ++ <\ \ =:=  (*`*); open Symbol

  structure ASM = MCS51Instruction
        and E = Expression
        and D = Declaration
        and P = ParsedNta

  val nmCycleConst = `"P"
  val nmCycleClk   = `"x"
  val nmAccum      = `"A"
  val nmCarry      = `"C"
  val nmIRAM       = `"IRAM"
  val nmERAM       = `"ERAM"
  val nmBITS       = `"BITS"
  val nmR          = Vector.tabulate (8, (fn i=> `("R"^Int.toString i)))

  val cycleConst = E.VarExpr (E.SimpleVar nmCycleConst)
  val cycleClk   = E.VarExpr (E.SimpleVar nmCycleClk)

  val one  = E.IntCExpr 1
  val zero = E.IntCExpr 0
  val maxbyte = E.IntCExpr 255
  
  local
    fun makeVar s       = E.VarExpr (E.SimpleVar (Atom.atom s))
    val accum = E.VarExpr (E.SimpleVar nmAccum)
    val carry = E.VarExpr (E.SimpleVar nmCarry)
    val iram  = E.SimpleVar nmIRAM
    val eram  = E.SimpleVar nmERAM
    val bits  = E.SimpleVar nmBITS

    fun regToAtom ASM.R0 = Vector.sub (nmR, 0)
      | regToAtom ASM.R1 = Vector.sub (nmR, 1)
      | regToAtom ASM.R2 = Vector.sub (nmR, 2)
      | regToAtom ASM.R3 = Vector.sub (nmR, 3)
      | regToAtom ASM.R4 = Vector.sub (nmR, 4)
      | regToAtom ASM.R5 = Vector.sub (nmR, 5)
      | regToAtom ASM.R6 = Vector.sub (nmR, 6)
      | regToAtom ASM.R7 = Vector.sub (nmR, 7)

    fun regToVar r      = E.VarExpr (E.SimpleVar (regToAtom r))
    fun subscriptVar (v,idxex) = E.VarExpr (E.SubscriptVar (v, idxex))
    fun directToVar d   = subscriptVar (iram, makeVar (ASM.directToString d))
    fun indirectToVar r = subscriptVar (iram, makeVar (ASM.indToString r))
    fun immToVar r      = makeVar (ASM.dataToString r)
    fun bitToVar b      = subscriptVar (bits, makeVar (ASM.bitToString b))

    fun assign (s,v,a) = [E.AssignExpr {var=s, aop=a, expr=v}]
  in
  fun addReset act = act @ assign (cycleClk, zero, E.AssignOp)

  fun makeAction (ASM.ADD_reg r)  = assign(accum,regToVar r,E.PlusEqOp)(*{{{1*)
    | makeAction (ASM.ADD_dir d)  = assign (accum, directToVar d,   E.PlusEqOp)
    | makeAction (ASM.ADD_ind r)  = assign (accum, indirectToVar r, E.PlusEqOp)
    | makeAction (ASM.ADD_imm d)  = assign (accum, immToVar d,      E.PlusEqOp)

    | makeAction (ASM.ADDC_reg r) = assign (accum, regToVar r,     E.PlusEqOp)
    | makeAction (ASM.ADDC_dir d) = assign (accum, directToVar d,  E.PlusEqOp)
    | makeAction (ASM.ADDC_ind r) = assign (accum, indirectToVar r,E.PlusEqOp)
    | makeAction (ASM.ADDC_imm d) = assign (accum, immToVar d,     E.PlusEqOp)

    | makeAction (ASM.SUBB_reg r) = assign (accum, regToVar r,     E.MinusEqOp)
    | makeAction (ASM.SUBB_dir d) = assign (accum, directToVar d,  E.MinusEqOp)
    | makeAction (ASM.SUBB_ind r) = assign (accum, indirectToVar r,E.MinusEqOp)
    | makeAction (ASM.SUBB_imm d) = assign (accum, immToVar d,     E.MinusEqOp)

    | makeAction (ASM.INC_acc)    = assign(accum,           one, E.PlusEqOp)
    | makeAction (ASM.INC_reg r)  = assign(regToVar r,      one, E.PlusEqOp)
    | makeAction (ASM.INC_dir d)  = assign(directToVar d,   one, E.PlusEqOp)
    | makeAction (ASM.INC_ind r)  = assign(indirectToVar r, one, E.PlusEqOp)

    | makeAction (ASM.DEC_acc)    = assign (accum,           one, E.MinusEqOp)
    | makeAction (ASM.DEC_reg r)  = assign (regToVar r,      one, E.MinusEqOp)
    | makeAction (ASM.DEC_dir d)  = assign (directToVar d,   one, E.MinusEqOp)
    | makeAction(ASM.DEC_ind r)   = assign (indirectToVar r, one, E.MinusEqOp)

    | makeAction (ASM.INC_DPTR)   = []
    | makeAction (ASM.MUL)        = []
    | makeAction (ASM.DIV)        = []
    | makeAction (ASM.DA)         = []

    | makeAction (ASM.ANL_reg r)  = assign (accum, regToVar r,    E.BAndEqOp)
    | makeAction (ASM.ANL_dir d)  = assign (accum, directToVar d, E.BAndEqOp)
    | makeAction (ASM.ANL_ind r)  = assign (accum,indirectToVar r,E.BAndEqOp)
    | makeAction (ASM.ANL_imm d)  = assign (accum, immToVar d,    E.BAndEqOp)
    | makeAction (ASM.ANL_A_dir d)= assign (directToVar d,accum,  E.BAndEqOp)
    | makeAction (ASM.ANL_dir_imm (d,i)) = assign (directToVar d, immToVar i,
                                               E.BAndEqOp)

    | makeAction (ASM.ORL_reg r)  = assign (accum, regToVar r,    E.BOrEqOp)
    | makeAction (ASM.ORL_dir d)  = assign (accum, directToVar d, E.BOrEqOp)
    | makeAction (ASM.ORL_ind r)  = assign (accum,indirectToVar r,E.BOrEqOp)
    | makeAction (ASM.ORL_imm d)  = assign (accum, immToVar d,    E.BOrEqOp)
    | makeAction (ASM.ORL_A_dir d)= assign (directToVar d, accum, E.BOrEqOp)
    | makeAction (ASM.ORL_dir_imm (d,i)) = assign (directToVar d, immToVar i,
                                               E.BOrEqOp)

    | makeAction (ASM.XRL_reg r)  = assign (accum, regToVar r,    E.BXorEqOp)
    | makeAction (ASM.XRL_dir d)  = assign (accum, directToVar d, E.BXorEqOp)
    | makeAction (ASM.XRL_ind r)  = assign (accum,indirectToVar r,E.BXorEqOp)
    | makeAction (ASM.XRL_imm d)  = assign (accum, immToVar d,    E.BXorEqOp)
    | makeAction (ASM.XRL_A_dir d)= assign (directToVar d, accum, E.BXorEqOp)
    | makeAction (ASM.XRL_dir_imm (d,i)) = assign (directToVar d, immToVar i,
                                                E.BXorEqOp)

    | makeAction (ASM.CLR)        = assign (accum, zero, E.AssignOp)
    | makeAction (ASM.CPL)        = []
    | makeAction (ASM.RL)         = []
    | makeAction (ASM.RLC)        = []
    | makeAction (ASM.RR)         = []
    | makeAction (ASM.RRC)        = []
    | makeAction (ASM.SWAP)       = []

    | makeAction (ASM.MOV_regToA r)= assign(accum, regToVar r,   E.AssignOp)
    | makeAction (ASM.MOV_dirToA d)= assign(accum, directToVar d,E.AssignOp)
    | makeAction (ASM.MOV_indToA r)= assign(accum,indirectToVar r,E.AssignOp)
    | makeAction (ASM.MOV_immToA d)= assign(accum, immToVar d,   E.AssignOp)

    | makeAction (ASM.MOV_AToReg   r)   = assign (regToVar r, accum,E.AssignOp)
    | makeAction (ASM.MOV_dirToReg (r,d)) = assign (regToVar r, directToVar d,
                                                    E.AssignOp)
    | makeAction (ASM.MOV_immToReg (r,d)) =assign(regToVar r, immToVar d,
                                                  E.AssignOp)

    | makeAction (ASM.MOV_AToDir d)       = assign(accum,directToVar d,E.AssignOp)
    | makeAction (ASM.MOV_regToDir (d,r)) = assign(directToVar d, regToVar r,
                                               E.AssignOp)
    | makeAction (ASM.MOV_dirToDir (dd,ds)) = assign(directToVar dd,
                                                directToVar ds, E.AssignOp)
    | makeAction (ASM.MOV_indToDir (d,r)) = assign(directToVar d,
                                              indirectToVar r, E.AssignOp)
    | makeAction (ASM.MOV_immToDir (dd,ds)) = assign(directToVar dd,
                                                immToVar ds, E.AssignOp)

    | makeAction (ASM.MOV_AToInd   r)     = assign(indirectToVar r, accum,
                                               E.AssignOp)
    | makeAction (ASM.MOV_dirToInd (r,d)) = assign(indirectToVar r,
                                               directToVar d, E.AssignOp)
    | makeAction (ASM.MOV_immToInd (r,d)) = assign(indirectToVar r, immToVar d,
                                               E.AssignOp)

    | makeAction (ASM.MOVDPTR data16) = []

    | makeAction (ASM.MOVC_DPTR)  = []
    | makeAction (ASM.MOVC_PC)    = []

    | makeAction (ASM.MOVX_From8 r)= assign (accum, subscriptVar (eram,
                                    makeVar (ASM.indToString r)),E.AssignOp)

    | makeAction (ASM.MOVX_From16)= []
    | makeAction (ASM.MOVX_To8 r) = assign (subscriptVar (eram, makeVar
                                       (ASM.indToString r)),accum,E.AssignOp)
    | makeAction (ASM.MOVX_To16)  = []

    | makeAction (ASM.PUSH _)     = []
    | makeAction (ASM.POP _)      = []

    | makeAction (ASM.XCH_rn _)   = []
    | makeAction (ASM.XCH_dir _)  = []
    | makeAction (ASM.XCH_ind _)  = []

    | makeAction (ASM.XCHD_ind _) = []

    | makeAction (ASM.CLR_c)      = assign (carry,E.falseExpr,E.AssignOp)
    | makeAction (ASM.CLR_bit b)  = assign (bitToVar b,E.falseExpr,E.AssignOp)
    | makeAction (ASM.SETB_c)     = assign (carry,E.trueExpr,E.AssignOp)
    | makeAction (ASM.SETB_bit b) = assign (bitToVar b,E.trueExpr,E.AssignOp)
    | makeAction (ASM.CPL_c)      = assign (carry,E.negate carry,E.AssignOp)
    | makeAction (ASM.CPL_bit b)  =assign(carry,E.negate(bitToVar b),E.AssignOp)

    | makeAction (ASM.ANL_bit _)  = []
    | makeAction (ASM.ANL_cbit _) = []
    | makeAction (ASM.ORL_bit _)  = []
    | makeAction (ASM.ORL_cbit _) = []

    | makeAction (ASM.MOV_cToBit _)= []
    | makeAction (ASM.MOV_bitToC _)= []

    | makeAction (ASM.ACALL _)    = []
    | makeAction (ASM.LCALL _)    = []
    | makeAction (ASM.RET)        = []
    | makeAction (ASM.RETI)       = []
    | makeAction (ASM.AJMP _)     = []
    | makeAction (ASM.LJMP _)     = []
    | makeAction (ASM.SJMP _)     = []
    | makeAction (ASM.JMP_DPTR)   = []
    | makeAction (ASM.JZ _)       = []
    | makeAction (ASM.JNZ _)      = []
    | makeAction (ASM.JC _)       = []
    | makeAction (ASM.JNC _)      = []
    | makeAction (ASM.JB _)       = []
    | makeAction (ASM.JNB _)      = []
    | makeAction (ASM.JBC (b, _))= assign (bitToVar b,E.falseExpr,E.AssignOp)

    | makeAction (ASM.CJNE_dirToA _)= []
    | makeAction (ASM.CJNE_immToA _)= []
    | makeAction (ASM.CJNE_immToReg _)= []
    | makeAction (ASM.CJNE_immToInd _)= []

    | makeAction (ASM.DJNZ_reg (r,_)) = assign (regToVar r, one, E.MinusEqOp)
    | makeAction (ASM.DJNZ_dir (d,_)) = assign (directToVar d, one, E.MinusEqOp)

    | makeAction (ASM.NOP)        = []
    (*}}}1*)

  fun jumpsTo (ASM.ACALL _)     = NONE (* could try... *)
    | jumpsTo (ASM.LCALL _)     = NONE (* ...to simulate... *)
    | jumpsTo ASM.RET           = NONE (* ...call... *)
    | jumpsTo ASM.RETI          = NONE (* ...stack. *)

    | jumpsTo (ASM.AJMP a)      = SOME (ASM.addr11ToString a)
    | jumpsTo (ASM.LJMP a)      = SOME (ASM.addr16ToString a)
    | jumpsTo (ASM.SJMP r)      = SOME (ASM.relToString r)
    | jumpsTo (ASM.JMP_DPTR)    = NONE
    | jumpsTo _                 = NONE

  fun guardCmp (l, cmp, r, dst) = let
      val g = E.RelExpr {left=l, rel=cmp, right=r}
    in SOME (g, ASM.relToString dst) end

  (* For jump expressions: returns a guard and label pair, the former
   * guarding a transition to the latter. *)
  fun jumpGuard (ASM.JZ rel)      = guardCmp (accum, E.EqOp, zero, rel)
    | jumpGuard (ASM.JNZ rel)     = guardCmp (accum, E.NeOp, zero, rel)
    | jumpGuard (ASM.JC rel)      = SOME (carry,          ASM.relToString rel)
    | jumpGuard (ASM.JNC rel)     = SOME (E.negate carry, ASM.relToString rel)
    | jumpGuard (ASM.JB (b,rel))  = SOME (bitToVar b,     ASM.relToString rel)
    | jumpGuard (ASM.JNB (b,rel)) = SOME (E.negate(bitToVar b),ASM.relToString rel)
    | jumpGuard (ASM.JBC (b,rel)) = SOME (bitToVar b, ASM.relToString rel)
    | jumpGuard (ASM.CJNE_dirToA (d,rel))= guardCmp (directToVar d,E.NeOp,accum,rel)
    | jumpGuard (ASM.CJNE_immToA (d,rel))= guardCmp (immToVar d,E.NeOp,accum,rel)
    | jumpGuard (ASM.CJNE_immToReg (r,d,rel)) = guardCmp (regToVar r, E.NeOp,
                                                      immToVar d, rel)
    | jumpGuard (ASM.CJNE_immToInd (r,d,rel)) = guardCmp (indirectToVar r, E.NeOp,
                                                      immToVar d, rel)
    | jumpGuard (ASM.DJNZ_reg (r,rel)) = guardCmp (regToVar r, E.NeOp, one, rel)
    | jumpGuard (ASM.DJNZ_dir (d,rel)) = guardCmp (directToVar d,E.NeOp,one,rel)
    | jumpGuard _ = NONE

  fun seqGuard (ASM.DJNZ_reg (r,rel)) =SOME (E.RelExpr {left=regToVar r,
                                                        rel=E.EqOp, right=one})
    | seqGuard (ASM.DJNZ_dir (d,rel)) =SOME (E.RelExpr {left=directToVar d,
                                                        rel=E.EqOp, right=one})
    | seqGuard act = Option.map (fn (jg,_)=>E.negate jg) (jumpGuard act)

  local
    val byte = E.INT (SOME (zero, maxbyte), E.NoQual)
    fun mkArray (n, ty) = E.ARRAY (ty,E.Type (E.INT
                                        (SOME (zero, E.IntCExpr (n - 1)),
                                        E.NoQual)))
    fun mkReg (r, rs) = (r, D.VarDecl {id=r, ty=byte, pos=D.nopos,
                                       initial=SOME (D.SimpleInit zero)}) :: rs

    val varMap = foldl AtomMap.insert' AtomMap.empty (
      (nmCycleConst,D.VarDecl {id=nmCycleConst, ty=E.INT (NONE, E.Const),
                               initial=NONE, pos=D.nopos})::
      (nmCycleClk,  D.VarDecl {id=nmCycleClk, ty=E.CLOCK, initial=NONE,
                               pos=D.nopos})::
      (nmAccum,     D.VarDecl {id=nmAccum, ty=byte, pos=D.nopos,
                               initial=SOME (D.SimpleInit zero)})::
      (nmCarry,     D.VarDecl {id=nmAccum, ty=E.BOOL E.NoQual, pos=D.nopos,
                               initial=SOME (D.SimpleInit E.falseExpr)})::
      (nmIRAM,      D.VarDecl {id=nmIRAM, ty=mkArray (128, byte),
                               initial=NONE, pos=D.nopos})::
      (nmERAM,      D.VarDecl {id=nmERAM, ty=mkArray (10,byte), initial=NONE,
                               pos=D.nopos})::
      (nmBITS,      D.VarDecl {id=nmBITS, ty=mkArray (128, E.BOOL E.NoQual),
                               initial=NONE, pos=D.nopos})::
      Vector.foldl mkReg [] nmR)
  in
  fun varToDecl nm = AtomMap.find (varMap, nm)
  end


  end (* local *)

  local
    val columnSep       = 280   (* distance between columns               *)
    val rowSep          = 120   (* distance between rows                  *)
    val invariantHorOff = 6     (* horizontal offset for invariant label  *)
    val invariantVerOff = 8     (* vertical offset for invariant label    *)

    infixr :::
    fun NONE ::: xs = xs | (SOME x) ::: xs = x::xs
  in
  fun actionConstraint (a, rel) = let
      val nc = ASM.numCycles a
      val t  = if nc = 1 then cycleConst
               else E.BinIntExpr {left=E.IntCExpr nc, bop=E.TimesOp,
                                  right=cycleConst}
    in E.RelExpr {left=cycleClk, rel=rel, right=t} end

  fun makeTimed ([], _) = P.Template.new ("", NONE)
    | makeTimed (instrs, {showinstrs, position, maxrows}) = let

      local val (currx, curry, n) = (ref 0, ref 0, ref 0)
            fun incx () = (currx := (!currx) + columnSep; curry := 0)
            fun incy () = (curry := (!curry) + rowSep)
      in fun incPos () = SOME (!currx, !curry)
                         before (n := ((!n) + 1) mod maxrows;
                                 if (!n) = 0 then incx () else incy ())
      end
      val nextPos = if position then incPos else (fn _ => NONE)

      fun makeSync act = if showinstrs
                         then SOME (Atom.atom ("'"^ASM.toString act^"'"),
                                    E.Output, [])
                         else NONE
      fun labelToLoc map (label, defaultloc) =
            case AtomMap.find (map, Atom.atom label) of
              NONE => (Util.warn ["missing location label: ",label]; defaultloc)
            | SOME l => l

      fun addJmpTrans map (loc, act) = let
          fun f (g, dstlabel) = let
              val dst = if dstlabel = "*" then loc
                        else labelToLoc map (dstlabel, loc)
              val guard = E.andexpr (g, actionConstraint (act, E.GeOp))
            in
              SOME (P.Transition {id=NONE, source=loc, target=dst,
                                  select=([], NONE), guard=(guard, NONE),
                                  sync=(makeSync act,NONE),
                                  update=(addReset (makeAction act),NONE),
                                  comments=(SOME (ASM.toString act), NONE),
                                  position=NONE, color=NONE, nails=[]})
            end
        in Option.mapPartial f (jumpGuard act) end

      fun addloc ((nmo, act), (template, map, lids, n)) = let
          val lid = P.Location.newId template
          val pos = nextPos ()
          val npos = Option.map (fn (x, y)=>(x - 25, y - 32)) pos
          val ipos = Option.map (fn (x, y)=>(x + invariantHorOff,
                                             y + invariantVerOff)) pos
          val l = P.Location {id=lid, position=pos, color=NONE,
                          name=(SOME ("s"^Int.toString n), npos),
                          invariant=(actionConstraint (act, E.LeOp), ipos),
                          comments=(nmo, NONE), urgent=false, committed=false}
          val map' = case nmo of
                       NONE   => map
                     | SOME s => AtomMap.insert (map, Atom.atom s, lid)
        in (P.Template.updLocation template l, map', (lid, act)::lids, n+1) end

      fun addFinal false args = args
        | addFinal true  (template, map, lids, n) = let
          val lid = P.Location.newId template
          val pos = nextPos ()
          val npos = Option.map (fn (x, y)=>(x - 25, y - 32)) pos
          val l = P.Location {id=lid, position=pos, color=NONE,
                    name=(SOME ("s"^Int.toString n), npos),
                    invariant=(E.trueExpr, NONE),
                    comments=(SOME "fin", NONE), urgent=false, committed=false}
        in
          (P.Template.updLocation template l, map, (lid, ASM.NOP)::lids, n+1)
        end

      val (template, locmap, rlids, _) = addFinal
            (not (isSome (jumpsTo (#2 (List.last instrs)))))
            (foldl addloc (P.Template.new ("",NONE), AtomMap.empty,[],0) instrs)

      val lids = rev rlids
      val template = P.Template.updInitial template (SOME (#1 (hd lids)))
      val locIdToPos = P.Location.toMap (template, valOf o P.Location.selPos)

      fun addSeqTrans (src, act, dst) = let
          val ac = actionConstraint (act, E.GeOp)
          val g  = case seqGuard act of
                     NONE    => ac
                   | SOME sg => E.andexpr (sg, ac)

          val (SOME (sp as (sx, _)), SOME (dp as (dx, _))) = (locIdToPos src,
                                                              locIdToPos dst)
          val nails = if sx = dx then [] else Layout.joinColumns (sp, dp)
        in
          P.Transition {id=NONE, source=src, target=dst,
                        select=([], NONE), guard=(g, NONE),
                        sync=(makeSync act,NONE),
                        update=(addReset (makeAction act),NONE),
                        comments=(SOME (ASM.toString act), NONE), position=NONE,
                        color=NONE, nails=nails}
        end

      fun addSeqTransitions map xs = let
          fun f [] = []
            | f [(loc,act)] = (case jumpsTo act of
                     NONE     => []
                   | SOME jdst=> [addSeqTrans(loc,act,labelToLoc map (jdst,loc))])
            | f ((src,act)::(ts as (dst,_)::_)) =
                    (case jumpsTo act of
                       NONE      =>addSeqTrans (src,act,dst)
                     | SOME jdst =>addSeqTrans (src,act,labelToLoc map (jdst,dst)))
                    ::f ts
        in f xs end

      val seqTrans = addSeqTransitions locmap lids
      val jmpTrans  = List.mapPartial (addJmpTrans locmap) lids
      val trans = (Layout.matrixTrans (locIdToPos, jmpTrans))
                  @ (map (Layout.positionLabels locIdToPos) seqTrans)

      val vars = foldl (fn (t, s)=>P.freeTransitionNames t ++ s)
                       (emptyset <+ nmCycleConst <+ nmCycleClk) trans

      val vardecls = AtomSet.foldl (fn (v,vs)=>varToDecl v:::vs) [] vars
      val decls = Environment.addDeclarations (P.noDeclaration,
                                               Environment.TemplateScope,
                                               vardecls)
    in
      P.Template.updDeclaration (P.Template.updTransitions template trans) decls
    end
  end (* local *)

end

