(* $Id$
 *
 * Functional symbol tables for types and values.
 *
 *)

(* Facts/restrictions about Uppaal (tested in 4.0.2, rev. 2491, August 2006):
   {{{1

    * Variables and functions share the same name space.
      
      int x =3;
      int x(int i) { return i + 2; } //FAILS: Duplicate definition error

    * Duplicate definitions only fail if both declarations are at the
      same scope.

      e.g.
      int test(int x) {
        int x = 8;        //FAILS: Duplicate definition error
        return 3;
      }

      int test(int x) {
        {int x = 8;}      //PASSES: New lexical scope
        return 3;
      }


    * Typedefs and variables share the same name space.
      
      typedef int x;
      int x;            //FAILS: Duplicate definition of identifier x

    * Variables and channels share the same name space.

      int x;
      chan x;   //FAILS: Duplicate definition of identifier x

    * Single pass processing.

        clock y[z * 7];
        const int z = 7;    //FAILS: Unknown identifier

        list l;   //FAILS: syntax error, unexpected T_ID, expecting T_ASSIGNMENT
        typedef int list[4];

    * Initialisers must be statically evaluable, and check is conservative.

        int y = 7;
        int z = y * 3;   //FAILS: Must be computable at compile time

        const int y = y;
        int z = y * 3;   //PASSES

    * Expressions in array declarations must be statically evaluable, and check
      is conservative.

        int y = 7;
        clock z[y];     //FAILS: Must be computable at compile time

        const int y = 7;
        clock z[y];     //PASSES

        Process P (int p) {
          clock x[p];   //FAILS: Must be computable at compile time
        }

        Process P (const int p) {
          clock x[p];   //PASSES
        }

    * Passing process parameters by reference is not considered as compuatable
      at compile time, even with const qualifier.

        Process P (const int& P) {
          clock x[p];   //FAILS: Must be compuatable at compile time  
        }

    * Structs cannot contain channel or clock fields.
      Despite a statement to the contrary in the help file: `There are 4
      predefined types: int, bool, clock, and chan. Array and record types can
      be defined over these and other types.'

        struct { int c; } x;              //PASSES
        struct { clock c; } x;            //FAILS: Invalid type in structure
        struct { chan c; } x;             //FAILS: Invalid type in structure
        typedef clock C; struct { C c; }  //FAILS: Invalid type in structure

    * Meta and Const qualifiers cannot be combined directly, but it is possible
      through typedefs:
        meta const int x;     //FAILS: syntax error, unexpected T_CONST
        const meta int x;     //FAILS: syntax error, unexpected T_META

        typedef const int Cint; meta Cint x;  //PASSES

    * Records cannot have constant fields (meta is ok):
        typedef struct
          { const int c; } S;  //FAILS: Constant fields not allowed in struct

        typedef const int Cint;
        typedef struct
          { Cint c; } S;       //FAILS: Constant fields not allowed in struct

    * Constants must have an initialiser.
        const int c;           //FAILS: Constants must have an initialiser

    * Functions can be used in initialisers. Functions may reference globals.

        const int z = 4;
        int f(int x) { return x + z }
        const int y = f(2);     //PASSES
  
        int z = 4;
        int f(int x) { return x + z } //PASSES
        const int y = f(2);           //FAILS: Must be computable at compile time

    * Functions may take clocks and channels as by-ref parameters.
        void f(clock& c) { c = 7; }  //PASSES
        void f(chan& c)  {}          //PASSES

    * Functions cannot return clocks, nor channels, nor references, nor arrays.
        clock g(clock& c) { return c; } //FAILS: Invalid return type
        chan  g(chan& c)  { return c; } //FAILS: Invalid return type

        clock& g(clock& c) { return c; } //FAILS: syntax error, unexpected '&'

    * Functions can return structs, which can be accessed:
        typedef struct { int x; int y; } Point;
        Point f(int z) { Point p = {z, z}; return p; } //PASSES

        on transition: f(3).x //PASSES

        typedef struct C { clock c; } C;  // FAILS: Invalid type in structure
        C f(C c) { return c; }

    TODO:
      * Since variables/functions and type abbreviations share the same name
        space, there probably isn't a need for paired environments?
      * `ref' is a terrible name for a record label.
  
 }}}1*)

signature ENVIRONMENT =
sig
  structure Expression : EXPRESSION
  structure Declaration : DECLARATION

  exception UndeclaredTypeName of string
  exception CannotStaticallyEvaluate of string
  exception DuplicateDefinition of string

  type ty = Expression.ty
  type expr = Expression.expr
  type decl = Declaration.decl
  type stmt = Declaration.stmt
  type symbol = Atom.atom

  (* Track scope so as to identify shared variables. *)
  datatype scopetag = GlobalScope       (* defined globally *)
                    | ParameterScope    (* template parameter *)
                    | TemplateScope     (* defined locally to template *)
                    | LocalScope        (* function parameter or local *)
                    | SelectScope       (* select id on transition *)
                    | BoundScope        (* bound variable (forall, exists) *)

  datatype enventry = VarEntry of {ty: ty,
                                   init: Declaration.initialiser option,
                                   ref: bool,
                                   scope: scopetag}
                    | FunEntry of {formals: {ty:ty, id:symbol, ref: bool} list,
                                   result: ty, body: stmt, scope: scopetag}

  type env
  val base_env  : env

  val addDeclarations : env * scopetag * Declaration.decl list -> env
  val addParameters   : env * scopetag * Declaration.param list -> env
  val typeEnvToString : env -> string
  val varEnvToString  : env -> string

  val expandTyIds     : env * ty -> ty

  val findVarExprType : env -> Expression.var -> ty option
  val findValType     : env -> symbol -> ty option

  val findVal         : env -> symbol -> enventry option

  val newId   : symbol * env -> symbol
  val usedIds : env -> AtomSet.set
  val addId   : scopetag -> (symbol * ty) * env -> env

  val writableVariables : env * scopetag -> symbol list
    (* Return a list of non-meta, non-const variables that are 
     * not channels nor clocks. *)

  val dupVariables : symbol list -> (env * scopetag option)
                     -> (symbol AtomMap.map * env)
    (* Each VarEntry meeting the given predicate is duplicated
     * in the returned environment. A map from names to
     * duplicate versions is also returned. Scopetags of duplicates
     * are updated if a new version is given. 
     *)

  val isEmpty : env -> bool

  (* Extract a list of all declared channel names *)
  val channels        : env -> symbol list

  (* Map values from the environment to a list. Elements are processed in the
     same order that they were created. Both are n*logn time, n space. *)
  val mapValues       : (symbol * enventry -> 'a option) -> env -> 'a list
  val mapTypes        : (symbol * scopetag * ty -> 'a option) -> env -> 'a list

  val filter          : (env * expr -> bool) -> env -> expr -> expr list
  (* Given a predicate, an environment, and an expression, filter returns a
   * list of the biggest subexpressions that satisfy the predicate (`biggest'
   * means that once a subexpression satisfies the predicates, its
   * (sub-)subexpressions * are not considered).
   *
   * Env is updated with bound variables as they are encountered.
   *)

end

