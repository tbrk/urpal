(* $Id$ *)

signature CMD_ENV = sig

  datatype direction = datatype Expression.direction

  type symbol = Atom.atom
  type symbolset = AtomSet.set

  datatype value = Template   of ParsedNta.template 
                 | Symbols    of AtomSet.set
                 | SymbolMap  of symbol AtomMap.map
                 | Actions    of ActionSet.set
                 | ActionMap  of (symbol * direction) ActionMap.map
                 | Fail       of string
                 | Success

  type t

  val fromNta       : ParsedNta.nta -> t
  val toNta         : t -> ParsedNta.nta

  val insert        : (symbol * value) * t -> t
  val remove        : t * symbol -> t * value
  val getValue      : t -> symbol -> value option

  val listTemplates : t -> ParsedNta.template list
  val listItems     : t -> (symbol * value) list

  val toString      : value -> string

  val globalDecl    : t -> Environment.env

  val symbolsToActions : AtomSet.set -> ActionSet.set
end

