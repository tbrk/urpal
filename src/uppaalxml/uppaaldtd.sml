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

structure UppaalDtd : UPPAAL_DTD =
struct
  open Dtd

  datatype tag = Nta | Imports | Declaration | Template | Name |
                 Parameter | Location | Init | Urgent | Committed |
                 Transition | Source | Target | Label | Nail |
                 Instantiation | System | UnknownTag

  datatype attribute = X | Y | Id | Color | Ref | Kind | UnknownAtt

  datatype labelkind = Invariant | Comments | Synchronisation |
                       Update | Guard | Select | UnknownKind

  val ntaGi           = UniChar.String2Data "nta" (*{{{1*)
  val importsGi       = UniChar.String2Data "imports"
  val declarationGi   = UniChar.String2Data "declaration"
  val templateGi      = UniChar.String2Data "template"
  val nameGi          = UniChar.String2Data "name"
  val parameterGi     = UniChar.String2Data "parameter"
  val locationGi      = UniChar.String2Data "location"
  val initGi          = UniChar.String2Data "init"
  val urgentGi        = UniChar.String2Data "urgent"
  val committedGi     = UniChar.String2Data "committed"
  val transitionGi    = UniChar.String2Data "transition"
  val sourceGi        = UniChar.String2Data "source"
  val targetGi        = UniChar.String2Data "target"
  val labelGi         = UniChar.String2Data "label"
  val nailGi          = UniChar.String2Data "nail"
  val instantiationGi = UniChar.String2Data "instantiation"
  val systemGi        = UniChar.String2Data "system"

  val xAtt            = UniChar.String2Data "x"
  val yAtt            = UniChar.String2Data "y"
  val idAtt           = UniChar.String2Data "id"
  val colorAtt        = UniChar.String2Data "color"
  val refAtt          = UniChar.String2Data "ref"
  val kindAtt         = UniChar.String2Data "kind"

  val kind_Invariant       = UniChar.String2Vector "invariant"
  val kind_Comments        = UniChar.String2Vector "comments"
  val kind_Synchronisation = UniChar.String2Vector "synchronisation"
  val kind_Update          = UniChar.String2Vector "assignment"
  val kind_Guard           = UniChar.String2Vector "guard"
  val kind_Select          = UniChar.String2Vector "select" (*}}}1*)

  fun initDtdTables () = let
      (* Create a new imperative Dtd every time. *)
      val dtd = Dtd.initDtdTables ()
      val _   = app (ignore o (Dtd.Element2Index dtd)) [ ntaGi,
                      importsGi, declarationGi, templateGi, nameGi,
                      parameterGi, locationGi, initGi, urgentGi,
                      committedGi, transitionGi, sourceGi, targetGi,
                      labelGi, nailGi, instantiationGi, systemGi ]
      val _   = app (ignore o (Dtd.AttNot2Index dtd)) []
    in dtd end

  local
    (*{{{1*)
    (* Relies on same indices being returned every time. *)
    val dtd = initDtdTables ()

    val ntaIdx           = Dtd.Element2Index dtd ntaGi
    val importsIdx       = Dtd.Element2Index dtd importsGi
    val declarationIdx   = Dtd.Element2Index dtd declarationGi
    val templateIdx      = Dtd.Element2Index dtd templateGi
    val nameIdx          = Dtd.Element2Index dtd nameGi
    val parameterIdx     = Dtd.Element2Index dtd parameterGi
    val locationIdx      = Dtd.Element2Index dtd locationGi
    val initIdx          = Dtd.Element2Index dtd initGi
    val urgentIdx        = Dtd.Element2Index dtd urgentGi
    val committedIdx     = Dtd.Element2Index dtd committedGi
    val transitionIdx    = Dtd.Element2Index dtd transitionGi
    val sourceIdx        = Dtd.Element2Index dtd sourceGi
    val targetIdx        = Dtd.Element2Index dtd targetGi
    val labelIdx         = Dtd.Element2Index dtd labelGi
    val nailIdx          = Dtd.Element2Index dtd nailGi
    val instantiationIdx = Dtd.Element2Index dtd instantiationGi
    val systemIdx        = Dtd.Element2Index dtd systemGi

    val xIdx             = Dtd.AttNot2Index dtd xAtt
    val yIdx             = Dtd.AttNot2Index dtd yAtt
    val idIdx            = Dtd.AttNot2Index dtd idAtt
    val colorIdx         = Dtd.AttNot2Index dtd colorAtt
    val refIdx           = Dtd.AttNot2Index dtd refAtt
    val kindIdx          = Dtd.AttNot2Index dtd kindAtt
    (*}}}1*)
  in
    fun idxToTag idx =
      if      idx=ntaIdx           then Nta
      else if idx=importsIdx       then Imports
      else if idx=declarationIdx   then Declaration
      else if idx=templateIdx      then Template
      else if idx=nameIdx          then Name
      else if idx=parameterIdx     then Parameter
      else if idx=locationIdx      then Location
      else if idx=initIdx          then Init
      else if idx=urgentIdx        then Urgent
      else if idx=committedIdx     then Committed
      else if idx=transitionIdx    then Transition
      else if idx=sourceIdx        then Source
      else if idx=targetIdx        then Target
      else if idx=labelIdx         then Label
      else if idx=nailIdx          then Nail
      else if idx=instantiationIdx then Instantiation
      else if idx=systemIdx        then System
      else                              UnknownTag

    fun idxToAtt idx =
      if      idx=xIdx             then X
      else if idx=yIdx             then Y
      else if idx=idIdx            then Id
      else if idx=colorIdx         then Color
      else if idx=refIdx           then Ref
      else if idx=kindIdx          then Kind
      else                              UnknownAtt

  end

  fun tagToStr Nta            = "Nta"
    | tagToStr Imports        = "Imports"
    | tagToStr Declaration    = "Declaration"
    | tagToStr Template       = "Template"
    | tagToStr Name           = "Name"
    | tagToStr Parameter      = "Parameter"
    | tagToStr Location       = "Location"
    | tagToStr Init           = "Init"
    | tagToStr Urgent         = "Urgent"
    | tagToStr Committed      = "Committed"
    | tagToStr Transition     = "Transition"
    | tagToStr Source         = "Source"
    | tagToStr Target         = "Target"
    | tagToStr Label          = "Label"
    | tagToStr Nail           = "Nail"
    | tagToStr Instantiation  = "Instantiation"
    | tagToStr System         = "System"
    | tagToStr UnknownTag     = "UnknownTag"

  fun vecToKind v = let
      fun matchv v' = UniChar.compareVector (v, v') = EQUAL
    in
           if matchv kind_Invariant       then Invariant
      else if matchv kind_Comments        then Comments
      else if matchv kind_Synchronisation then Synchronisation
      else if matchv kind_Update          then Update
      else if matchv kind_Guard           then Guard
      else if matchv kind_Select          then Select
      else                                     UnknownKind
    end

end

