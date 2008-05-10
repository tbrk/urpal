(* $Id$ *)
structure UppaalHooks =
struct

  open IgnoreHooks

  structure U = TextNta
        and D = UppaalDtd

  exception ParseError of string

  datatype attributeval = A_X of int
                        | A_Y of int
                        | A_Id of int
                        | A_IdRef of int
                        | A_Color of string
                        | A_Label of D.labelkind

  datatype texttag = CTImports | CTInstantiation | CTSystem | CTDeclaration
  datatype idtag = CIInit | CISource | CITarget

  datatype content = CData of UniChar.Vector
                   | CText of texttag * string option
                   | CLocId of idtag * U.locId
                   | CName of U.label
                   | CLabel of D.labelkind * U.label
                   | CNta of U.nta

                   | CTemplate of U.template
                   | CParameter of U.label

                   | CLocation of U.location
                   | CUrgent
                   | CCommitted

                   | CTransition of U.transition
                   | CNail of U.pos

  type stacked = (D.tag * attributeval list * content list) list

  type AppData = bool * content list * stacked
  type AppFinal = U.nta

  val initData = (false, [], []) : AppData

  local (* Auxilliary functions *)
    (*{{{1*)
    open HookData Base

    val stripWS = Substring.string o
        (Substring.dropr Char.isSpace) o (Substring.dropl Char.isSpace) o
        Substring.full

    val vec2Str = UniChar.Latin2String o UniChar.Vector2Data

    fun cleanAtt (idx, presval, _) =
      let (*{{{2*)
        val typedval = case presval of
                         AP_PRESENT (_, _, tv) => tv
                       | AP_DEFAULT (_, _, tv) => tv
                       | _                     => NONE

        fun getInt label (SOME (AV_ID i))    = SOME (label i)
          | getInt label (SOME (AV_IDREF i)) = SOME (label i)
          | getInt _ _                       = NONE

        fun getIntFromVec label (SOME (AV_CDATA v)) =
                                       (case Int.fromString (vec2Str v) of
                                          SOME i    => SOME (label i) 
                                        | NONE      => NONE)
          | getIntFromVec _ _                       = NONE

        fun getVec label (SOME (AV_CDATA v)) = SOME (label (vec2Str v))
          | getVec _ _                       = NONE

        fun getKind (SOME (AV_CDATA v)) = let val k = D.vecToKind v in
                                            if k=D.UnknownKind then NONE
                                            else SOME (A_Label (D.vecToKind v))
                                          end
          | getKind _                   = NONE
      in
        case D.idxToAtt idx of
          D.X          => getIntFromVec A_X typedval
        | D.Y          => getIntFromVec A_Y typedval
        | D.Id         => getInt A_Id typedval
        | D.Ref        => getInt A_IdRef typedval
        | D.Color      => getVec A_Color typedval
        | D.Kind       => getKind typedval
        | D.UnknownAtt => NONE
      end (*}}}2*)

    fun cleanAtts []      = []
      | cleanAtts (x::xs) = case cleanAtt x of
                              NONE   => cleanAtts xs
                            | SOME v => v::(cleanAtts xs)

    fun keepData D.Imports       = true
      | keepData D.Declaration   = true
      | keepData D.Name          = true
      | keepData D.Parameter     = true
      | keepData D.Label         = true
      | keepData D.Instantiation = true
      | keepData D.System        = true
      | keepData _               = false

    fun attsToKind (A_Label k::_) = k
      | attsToKind (_::xs)        = attsToKind xs
      | attsToKind _              = raise ParseError "Missing kind attribute."

    fun attsToLocId ((A_Id i)::_) = U.LocId i
      | attsToLocId (_::xs)       = attsToLocId xs
      | attsToLocId _             = raise ParseError "Missing id attribute."

    fun attsToTransId ((A_Id i)::_) = SOME (U.TransId i)
      | attsToTransId (_::xs)       = attsToTransId xs
      | attsToTransId _             = NONE

    fun attsToLocRef (A_IdRef i::_) = U.LocId i
      | attsToLocRef (_::xs)        = attsToLocRef xs
      | attsToLocRef _              = raise ParseError "Missing ref attribute."

    fun attsToColor (A_Color s::_) = SOME s
      | attsToColor (_::xs)        = attsToColor xs
      | attsToColor []             = NONE

    fun attsToPos atts = let
        fun findboth _ (SOME x, SOME y)  = SOME (x,y)
          | findboth (A_X x::xs) (_, oy) = findboth xs (SOME x, oy)
          | findboth (A_Y y::xs) (ox, _) = findboth xs (ox, SOME y)
          | findboth (_::xs) (ox, oy)    = findboth xs (ox, oy)
          | findboth [] _                = NONE
      in
        findboth atts (NONE, NONE)
      end

    fun getLocation (CLocation x)     = SOME x  | getLocation _ = NONE
    fun getTransition (CTransition x) = SOME x  | getTransition _ = NONE
    fun getTemplate (CTemplate x)     = SOME x  | getTemplate _ = NONE
    fun getNail (CNail x)             = SOME x  | getNail _ = NONE

    fun isUrgent CUrgent = true       | isUrgent _ = false
    fun isCommitted CCommitted = true | isCommitted _ = false

    fun getName (CName l::_) = l
      | getName (_::xs)      = getName xs
      | getName []           = (NONE, NONE)

    fun getParameter (CParameter l::_) = l
      | getParameter (_::xs)           = getParameter xs
      | getParameter []                = (NONE, NONE)

    fun getOText t (CText (tag, d)::xs) = if t=tag then d
                                          else getOText t xs
      | getOText t (_::xs)              = getOText t xs
      | getOText _ _                    = NONE

    fun getLocId t (CLocId (tag, d)::xs) = if t=tag then SOME d
                                           else getLocId t xs
      | getLocId t (_::xs)               = getLocId t xs
      | getLocId _ _                     = NONE

    fun getLabel t (CLabel (tag, d)::xs) = if t=tag then d
                                           else getLabel t xs
      | getLabel t (_::xs)               = getLabel t xs
      | getLabel _ _                     = (NONE, NONE)
    
    fun groupContent c = let
        fun justCData (CData vec) = SOME vec
          | justCData _           = NONE
      in case List.mapPartial justCData c of
           [] => NONE
         | xs => SOME (stripWS (vec2Str (Vector.concat (rev xs))))
      end

  (*}}}1*)
  in
    fun hookError     (data, ((f, l, c), error)) = let in
        (*{{{1*)
        raise ParseError (String.concat
                [f, ":", Int.toString l, ":", Int.toString c, ":",
                 ListFormat.fmt {init="", final="", sep=" ", fmt=fn x=>x}
                                                (Errors.errorMessage error)])
      end (*}}}1*)

    fun hookWarning   (data, ((f, l, c), warning)) = let in
        (*{{{1*)
        data before (
          Util.warn [f, ":", Int.toString l, ":", Int.toString c, ":",
          ListFormat.fmt {init="", final="", sep=" ", fmt=fn x=>x}
                         (Errors.warningMessage warning)])
      end (*}}}1*)

    fun hookStartTag  ((keep, content, stack), (_,elem,atts,_,empty)) = let
        val tag = D.idxToTag elem         (*{{{1*)
        val attribs = cleanAtts atts
      in
        if not empty
        then (Util.debugIndent (Settings.VeryDetailed,
                                fn()=>["<", D.tagToStr tag, ">"]);
              (keepData tag, [], (tag, attribs, content)::stack))
        else let
               val _ = Util.debugVeryDetailed (fn()=>["<",D.tagToStr tag,">"])
               val c = case tag of
                          D.Init      => CLocId (CIInit, attsToLocRef attribs)
                        | D.Urgent    => CUrgent
                        | D.Committed => CCommitted
                        | D.Source    => CLocId (CISource, attsToLocRef attribs)
                        | D.Target    => CLocId (CITarget, attsToLocRef attribs)
                        | D.Nail      => CNail (valOf (attsToPos attribs))
                        | _         => raise ParseError "Unexpected Empty"
             in (keep, c::content, stack) end
      end (*}}}1*)
        
    fun hookEndTag ((_, innercontent, (tag, attribs, outercontent)::stack),
                    (_, idx, _)) = let
        val _ = Util.debugOutdent (Settings.VeryDetailed,
                                   fn()=>["</", D.tagToStr tag, ">"])
        val c = case tag of (*{{{1*)
              D.Nta           => CNta (U.Nta {
                       imports       = getOText CTImports innercontent,
                       declaration   = getOText CTDeclaration innercontent,
                       templates     = rev (List.mapPartial
                                              getTemplate innercontent),
                       instantiation = getOText CTInstantiation innercontent,
                       system        = valOf (getOText CTSystem innercontent)
                                    })
            | D.Imports       => CText (CTImports, groupContent innercontent)
            | D.Declaration   => CText (CTDeclaration, groupContent innercontent)
            | D.Template      => CTemplate (U.Template {
                           name        = (fn (x, y)=> (valOf x, y))
                                         (getName innercontent),
                           parameter   = getParameter innercontent,
                           declaration = getOText CTDeclaration innercontent,
                           initial     = getLocId CIInit innercontent,
                           locations   = rev (List.mapPartial
                                                getLocation innercontent),
                           transitions = rev (List.mapPartial
                                                getTransition innercontent)
                               })
            | D.Name          => CName (groupContent innercontent,
                                        attsToPos attribs)
            | D.Parameter     => CParameter (groupContent innercontent,
                                           attsToPos attribs)
            | D.Location      => CLocation (U.Location {
                                 id        = attsToLocId attribs,
                                 position  = attsToPos attribs,
                                 color     = attsToColor attribs,
                                 name      = getName innercontent,
                                 invariant = getLabel D.Invariant innercontent,
                                 comments  = getLabel D.Comments innercontent,
                                 urgent    = List.exists isUrgent innercontent,
                                 committed = List.exists isCommitted
                                                         innercontent
                               })
            | D.Transition    => CTransition (U.Transition {
                                 id        = attsToTransId attribs,
                                 source    = valOf (getLocId CISource
                                                             innercontent),
                                 target    = valOf (getLocId CITarget
                                                             innercontent),
                                 select    = getLabel D.Select innercontent,
                                 guard     = getLabel D.Guard innercontent,
                                 sync      = getLabel D.Synchronisation
                                                      innercontent,
                                 update    = getLabel D.Update innercontent,
                                 comments  = getLabel D.Comments innercontent,
                                 position  = attsToPos attribs,
                                 color     = attsToColor attribs,
                                 nails     = rev (List.mapPartial
                                                    getNail innercontent)
                               })
            | D.Label         => CLabel (attsToKind attribs,
                                       (groupContent innercontent,
                                        attsToPos attribs))
            | D.Instantiation => CText (CTInstantiation,
                                      groupContent innercontent)
            | D.System        => CText (CTSystem, groupContent innercontent)
            | _               => raise ParseError "Unexpected Endtag"
      in
        (false, c::outercontent, stack)
      end (*}}}1*)

    fun hookData (data as (keep, content, stack), (_, vec, _)) =
        if keep then (keep, (CData vec)::content, stack)
        else data

    fun hookCharRef (data as (keep, content, stack), (_, c, _)) =
       if keep then (keep, CData (Vector.fromList [c])::content, stack)
       else data

    fun hookFinish (_, [CNta nta], _) = nta
      | hookFinish _                  = raise ParseError "NTA not parsed"

  end
end

