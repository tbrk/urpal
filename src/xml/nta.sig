(* $Id$ *)

signature NTA =
sig
  type pos = int * int
  type label = string option * pos option

  include NTA_TYPES

  datatype locId = LocId of int
  datatype location = Location of {
                        id          : locId,
                        position    : pos option,
                        color       : string option,
                        name        : label,
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

  val selTemplates   : nta -> template list
  val updTemplates   : nta -> template list -> nta
  val selDeclaration : nta -> declaration
  val updDeclaration : nta -> declaration -> nta

  val addTemplates   : nta -> template list -> nta
  (* NB: Does not modify the global definitions. Call renumber before
   *     writing to file.*)

  val getTemplate    : nta -> string -> template option

  val renumber       : nta -> nta
  (* Ensure the uniqueness of all transition and location ids, by remapping
   * them as necessary (thus invalidating all external ids) *)

  structure Template : sig
    val new            : (string * locId option) -> template

    val transform      : {m11:real,m12:real,m21:real,m22:real,tx:real,ty:real}
                         -> template -> template
    (* Transform each pos value via:  | m11  m12  0 |
     *                                | m22  m22  0 |
     *                                | tx   ty   1 |
     * Intermediate calculations are done with reals and then rounded. *)

    val prune          : template -> template
    (* Remove locations with no incoming or outgoing transitions.
       Remove transitions where either of the source or destination id is
       not present in list of locations. *)

    val selName        : template -> string 
    val updName        : template -> string -> template

    val selInitial     : template -> locId option
    val updInitial     : template -> locId option -> template

    val selLocations   : template -> location list
    val updLocations   : template -> location list -> template
    val updLocation    : template -> location -> template
      (* tries to replace a matching location id within the template *)

    val mapLocation    : template -> locId * (location -> location) -> template
    
    val selTransitions : template -> transition list
    val updTransitions : template -> transition list -> template
    val addTransitions : template -> transition list -> template

    val selDeclaration : template -> declaration
    val updDeclaration : template -> declaration -> template

    val names          : nta -> string list

    val map            : (template -> template) -> nta -> nta
    val mapPartial     : (template -> template option) -> nta -> nta
  end

  structure Location : sig
    val isCommitted    : location -> bool
    val isUrgent       : location -> bool
    val shift          : {xoff: int, yoff: int} option -> location -> location

    val selName        : location -> string option
    val updName        : location -> string option -> location
    val selColor       : location -> string option
    val updColor       : location -> string option -> location

    val selPos         : location -> pos option
    val updPos         : location -> pos option -> location

    val selId          : location -> locId

    val map            : (location -> location) -> template -> template
    val mapPartial     : (location -> location option) -> template -> template

    val new            : template -> string -> template * location
    val newId          : template -> locId

    val nameToId       : template -> string -> locId option

    val mkCommitted    : location -> location
    val mkUrgent       : location -> location
  end

  structure Transition : sig
    val map            : (transition -> transition) -> template -> template
    val mapPartial     : (transition -> transition option)
                         -> template -> template
    
    val selSync        : transition -> sync
    val updSync        : transition -> sync -> transition

    val selNails       : transition -> pos list
    val updNails       : transition -> pos list -> transition

    val selEndPoints   : transition -> locId * locId
    val updEndPoints   : transition -> locId * locId -> transition

    val new            : locId * locId -> transition
  end

end

