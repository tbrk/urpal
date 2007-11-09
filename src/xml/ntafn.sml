(* $Id$ *)

functor NtaFn (T : NTA_TYPES) : NTA =
struct
  type pos = int * int
  type label = string option * pos option

  type invariant     = T.invariant
  type select        = T.select
  type guard         = T.guard
  type sync          = T.sync
  type update        = T.update
  type parameter     = T.parameter
  type declaration   = T.declaration
  type imports       = T.imports
  type instantiation = T.instantiation
  type system        = T.system

  val noInvariant    = T.noInvariant
  val noSelect       = T.noSelect
  val noGuard        = T.noGuard
  val noSync         = T.noSync
  val noUpdate       = T.noUpdate

  val noParameter     = T.noParameter
  val noDeclaration   = T.noDeclaration
  val noImports       = T.noImports
  val noInstantiation = T.noInstantiation
  val noSystem        = T.noSystem

  datatype locId = LocId of int
  datatype location = Location of {
                   id          : locId,
                   position    : pos option,
                   color       : string option,
                   name        : string option    * pos option,
                   invariant   : invariant * pos option,
                   comments    : label,
                   urgent      : bool,
                   committed   : bool
                  }

  datatype transId = TransId of int
  datatype transition = Transition of {
                     id        : transId option,
                     source    : locId,
                     target    : locId,

                     select    : select * pos option,
                     guard     : guard * pos option,
                     sync      : sync * pos option,
                     update    : update * pos option,

                     comments  : label,
                     position  : pos option,
                     color     : string option,
                     nails     : pos list
                    }

  datatype template = Template of {
                   name        : string * pos option,
                   parameter   : parameter * pos option,
                   declaration : declaration,
                   initial     : locId option,
                   locations   : location list,
                   transitions : transition list
                  }

  datatype nta = Nta of {
              imports       : imports,
              declaration   : declaration,
              templates     : template list,
              instantiation : instantiation,
              system        : system
             }

  fun selTemplates (Nta {templates, ...}) = templates
  fun updTemplates (Nta {imports, declaration, instantiation,
                         system, ...}) tplates =
        Nta {imports=imports, declaration=declaration, templates=tplates,
             instantiation=instantiation, system=system}

  fun selDeclaration (Nta {declaration, ...}) = declaration
  fun updDeclaration (Nta {imports, instantiation, templates,
                           system, ...}) decl =
        Nta {imports=imports, declaration=decl, templates=templates,
             instantiation=instantiation, system=system}


  fun addTemplates (Nta {imports, declaration, templates,
                           instantiation, system}) ts
      = Nta {imports=imports, declaration=declaration,
             templates=templates @ ts,
             instantiation=instantiation, system=system}

  structure Template = struct
    fun new (name, initial) = Template {
                                name=(name, NONE),
                                parameter=(noParameter, NONE),
                                declaration=noDeclaration,
                                initial=initial, locations=[], transitions=[]}

    fun transform {m11, m12, m21, m22,  tx,  ty}
           (Template {name=(namev, namep), parameter=(paramv, paramp),
                      declaration, initial, locations, transitions}) =
      let
        open Real;

        fun tr (x, y) = let val (x, y) = (fromInt x, fromInt y)
          in
            (round (m11 * x + m12 * y + tx),
             round (m21 * x + m22 * y + ty))
          end
        val tro = Option.map tr

        fun trLoc (Location {id, position, color,
                             name=(namev, namep), invariant=(invv, invp),
                             comments, urgent, committed})
            = Location {id=id, position=tro position, color=color,
                        name=(namev, tro namep), invariant=(invv, tro invp),
                        comments=comments, urgent=urgent, committed=committed}

        fun trTrans (Transition {id, source, target, select=(selv, selp),
                                 guard=(gv, gp), sync=(synv, synp),
                                 update=(updv, updp), comments,position,color,
                                 nails})
            = Transition {id=id, source=source, target=target,
                          select=(selv, tro selp), guard=(gv, tro gp),
                          sync=(synv, tro synp), update=(updv, tro updp),
                          comments=comments, position=tro position,
                          color=color, nails=map tr nails}
      in
        Template {name=(namev, tro namep),
                  parameter=(paramv, tro paramp),
                  declaration=declaration,
                  initial=initial,
                  locations=map trLoc locations,
                  transitions=map trTrans transitions}
      end

    local
      structure ISet = IntBinarySet
    in
    fun prune (Template {name, parameter, declaration, initial,
                         locations, transitions}) = let

        fun trRef (Transition{source=LocId s, target=LocId t, ...}, m) =
              ISet.add (ISet.add (m, s), t)

        fun trLoc (Location {id=LocId l, ...}, m) = ISet.add (m, l)
        
        val withTrans = foldl trRef ISet.empty transitions
        val hasLoc = foldl trLoc ISet.empty locations

        fun lf (loc as Location {id=LocId l, ...}) =
                if ISet.member (withTrans, l) then SOME loc else NONE
        
        fun tf (tr as Transition {source=LocId s, target=LocId t, ...}) =
                  if ISet.member (hasLoc, s) andalso ISet.member (hasLoc, t)
                  then SOME tr else NONE

        val initial' =
          case Option.map (fn (LocId l)=>ISet.member (hasLoc, l)) initial of
            SOME true => initial | _ => NONE
      in
        Template {name=name,
                  parameter=parameter,
                  declaration=declaration,
                  initial=initial',
                  locations=List.mapPartial lf locations,
                  transitions=List.mapPartial tf transitions}
      end
    end (* local *)

    fun selName (Template {name=(name, _), ...}) = name
    fun updName (Template {name=(_, nameP), parameter, declaration, initial,
                           locations, transitions}) name =
        Template {name=(name, nameP), parameter=parameter,
                  declaration=declaration, initial=initial,
                  locations=locations, transitions=transitions}

    fun selInitial (Template {initial, ...}) = initial
    fun updInitial (Template {name, parameter, declaration,
                              locations, transitions, ...}) l =
        Template {name=name, parameter=parameter,
                  declaration=declaration, initial=l,
                  locations=locations, transitions=transitions}

    fun selDeclaration (Template {declaration, ...}) = declaration
    fun updDeclaration (Template {name, parameter, initial,
                                  locations, transitions, ...}) decl =
        Template {name=name, parameter=parameter,
                  declaration=decl, initial=initial,
                  locations=locations, transitions=transitions}

    fun selLocations (Template {locations, ...}) = locations
    fun updLocations (Template {name, parameter, declaration, initial,
                                transitions, ...}) locs =
        Template {name=name, parameter=parameter,
                  declaration=declaration, initial=initial,
                  locations=locs, transitions=transitions}
    
    fun updLocation (tplate as Template {locations, ...})
                    (loc as Location {id, ...}) = let
        fun f [] = [loc]
          | f ((loc' as Location {id=id', ...})::ls) =
                      if id=id' then loc::ls else loc'::f ls
      in updLocations tplate (f locations) end

    fun mapLocation (tplate as Template {locations, ...})
                    (l, fl) = let
        fun m (loc as Location {id, ...}) = if id = l then fl loc else loc
      in updLocations tplate (map m locations) end

    fun selTransitions (Template {transitions, ...}) = transitions
    fun updTransitions (Template {name, parameter, declaration, initial,
                                  locations, ...}) trans =
        Template {name=name, parameter=parameter,
                  declaration=declaration, initial=initial,
                  locations=locations, transitions=trans}
    fun addTransitions (Template {name, parameter, declaration, initial,
                                  locations, transitions}) trans =
        Template {name=name, parameter=parameter,
                  declaration=declaration, initial=initial,
                  locations=locations, transitions=trans@transitions}

    fun names (Nta {templates, ...}) = map selName templates
    fun map f nta = updTemplates nta (List.map f (selTemplates nta))
    fun mapPartial f nta = updTemplates nta (List.mapPartial f
                              (selTemplates nta))

    local
      structure IMap = IntBinaryMap
    in
    fun renumber (firstid, Template {name, parameter, declaration, initial,
                                     locations, transitions}) = let

        fun lid (Location{id=LocId i,...}, (m,ni)) =(IMap.insert (m,i,ni),ni+1)
        fun tid (Transition {id=SOME (TransId i),...}, (m, ni)) =
                                                    (IMap.insert (m,i,ni),ni+1)
          | tid (_, args) = args
        
        val (locMap, ni) = foldl lid (IMap.empty, firstid) locations
        val (trMap, nextid)  = foldl tid (IMap.empty, ni) transitions
        
        fun remapLoc (LocId i) = case IMap.find (locMap, i) of
                                   NONE    => (Util.warn ["bad location id"];
                                               LocId i)
                                 | SOME i' => LocId i'

        fun remapTrans (TransId i) = TransId (valOf (IMap.find (trMap, i)))

        fun rl (Location {id, position, color, name, invariant, comments,
                          urgent, committed})
              = Location {id=remapLoc id,
                          position=position, color=color, name=name,
                          invariant=invariant, comments=comments,
                          urgent=urgent, committed=committed}

        fun rt (Transition {id, source, target, select, guard, sync, update,
                            comments, position, color, nails})
              = Transition {id=Option.map remapTrans id,
                            source=remapLoc source,
                            target=remapLoc target,
                            select=select, guard=guard, sync=sync,
                            update=update, comments=comments,
                            position=position, color=color, nails=nails}
      in
        (Template {name=name,
                   parameter=parameter,
                   declaration=declaration,
                   initial=Option.map remapLoc initial,
                   locations=List.map rl locations,
                   transitions=List.map rt transitions}, nextid)
      end
    end (* local *)
  end (* Template *)

  structure Location = struct
    fun map f t = Template.updLocations t
                            (List.map f (Template.selLocations t))
    fun mapPartial f t = Template.updLocations t
                            (List.mapPartial f (Template.selLocations t))

    fun isCommitted (Location {committed, ...}) = committed
    fun isUrgent    (Location {urgent, ...})    = urgent

    fun selName (Location {name=(name, _),...}) = name
    fun updName (Location {id, position, color, name=(_, nameP),
                           invariant, comments, urgent, committed}) name' =
          Location {id=id,
                    position=position,
                    color=color,
                    name=(name', nameP),
                    invariant=invariant,
                    comments=comments,
                    urgent=urgent,
                    committed=committed}

    fun selColor (Location {color, ...}) = color
    fun updColor (Location {id, position, name, invariant, comments,
                            urgent, committed, ...}) color'
        = Location {id=id,
                    position=position,
                    color=color',
                    name=name,
                    invariant=invariant,
                    comments=comments,
                    urgent=urgent,
                    committed=committed}

    fun selPos (Location {position, ...}) = position
    fun updPos (Location {id, name, invariant, comments, urgent, committed,
                          color, ...}) pos'
        = Location {id=id, position=pos', color=color, name=name,
                    invariant=invariant, comments=comments,
                    urgent=urgent, committed=committed}

    fun selId (Location {id, ...}) = id

    fun mkCommitted (Location {id, position, name, invariant, comments,
                               urgent, color, ...})
        = Location {id=id,position=position,color=color,name=name,
                    invariant=invariant,comments=comments,urgent=urgent,
                    committed=true}

    fun mkUrgent (Location {id, position, name, invariant, comments,
                            committed, color, ...})
        = Location {id=id,position=position,color=color,name=name,
                    invariant=invariant,comments=comments,committed=committed,
                    urgent=true}

    fun shift NONE l = l
      | shift (SOME {xoff,yoff}) (l as Location{position=NONE,...}) = l
      | shift (SOME {xoff,yoff}) (l as Location{id, position=SOME pos,
                                                color=color, name=name,
                                                invariant=(inv, invP),
                                                comments, urgent, committed}) =
        let
          fun sh (x, y) = (x + xoff, y + yoff)
        in
          Location {id=id,
                    position=SOME (sh pos),
                    color=color,
                    name=name,
                    invariant=(inv, Option.map sh invP),
                    comments=comments,
                    urgent=urgent,
                    committed=committed}
        end

    fun newId (Template {locations, ...}) = let
        fun maxLoc (Location {id=LocId i, ...}, m) = Int.max (m, i)
      in LocId ((foldl maxLoc 0 locations) + 1) end

    fun newName (basename, Template {locations, ...}) = let
        fun f (Location {name=(NONE,_), ...},s)   = s
          | f (Location {name=(SOME n,_), ...},s) = AtomSet.add (s,Atom.atom n)

        val usednames = foldl f AtomSet.empty locations
      in Atom.toString (Symbol.getNewName (Atom.atom basename, usednames)) end

    fun new tplate name = let
        val name' = if name="" then NONE
                    else SOME (newName (name, tplate))

        val newLoc = Location {
                       id=newId tplate,
                       position=NONE,
                       color=Settings.newColor (),
                       name=(name', NONE),
                       invariant=(T.noInvariant, NONE),
                       comments=(NONE, NONE),
                       urgent=false,
                       committed=false
                     }
      in
        (Template.updLocations tplate (newLoc::Template.selLocations tplate),
         newLoc)
      end

    fun nameToId (Template {locations, ...}) name = let
        fun f (Location {name=(SOME name', _), ...}) = (name=name') | f _ = false
      in Option.map selId (List.find f locations) end
    
  end (* Location *)

  structure Transition = struct
    fun map f t = Template.updTransitions t
                              (List.map f (Template.selTransitions t))
    fun mapPartial f t = Template.updTransitions t
                              (List.mapPartial f (Template.selTransitions t))

    fun selSync (Transition {sync=(syn, _), ...}) = syn
    fun updSync (Transition {id, source, target, select, guard,
                             sync=(_, synP), update, comments,
                             position, color, nails}) syn' =
        Transition {id=id, source=source, target=target, select=select,
                     guard=guard, sync=(syn', synP),
                     update=update, comments=comments, position=position,
                     color=color, nails=nails}
    
    fun selEndPoints (Transition {source, target, ...}) = (source, target)
    fun updEndPoints (Transition {id, select, guard, sync, update, comments,
                                  position, color, nails, ...})
                     (source', target')
        = Transition {id=id, source=source', target=target', select=select,
                      guard=guard, sync=sync, update=update, comments=comments,
                      position=position, color=color, nails=nails}

    fun selNails (Transition {nails, ...}) = nails
    fun updNails (Transition {id, source, target, select, guard, sync, update,
                              comments, position, color, ...}) nails'
        = Transition {id=id, source=source, target=target, select=select,
                      guard=guard, sync=sync, update=update, comments=comments,
                      position=position, color=color, nails=nails'}

    fun newId (Template {transitions, ...}) = let
        fun maxTrans (Transition {id=NONE, ...}, m)          = m
          | maxTrans (Transition {id=SOME(TransId i),...},m) = Int.max (m,i)
      in TransId ((foldl maxTrans 0 transitions) + 1) end

    fun new (src, tgt) = Transition {id=NONE, source=src, target=tgt,
                                     select=(noSelect, NONE),
                                     guard=(noGuard, NONE),
                                     sync=(noSync, NONE),
                                     update=(noUpdate, NONE),
                                     comments=(NONE, NONE),
                                     position=NONE,
                                     color=Settings.newColor (),
                                     nails=[]}
  end (* Transition *)

  fun renumber (Nta {imports,declaration,templates,instantiation,system}) = let
      fun f (_, []) = []
        | f (id, tp::tps)= let val (tp', id') = Template.renumber (id, tp)
                           in tp'::f (id', tps) end
    in
      Nta {imports=imports, declaration=declaration,
           templates=f (0, templates),
           instantiation=instantiation, system=system}
    end

  fun getTemplate nta n = List.find (fn t=> Template.selName t = n)
                                    (selTemplates nta)

end

