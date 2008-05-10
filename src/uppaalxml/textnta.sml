(* $Id$ *)

structure TextNta = NtaFn (type invariant     = string option
                            and select        = string option
                            and guard         = string option
                            and sync          = string option
                            and update        = string option
                            and parameter     = string option
                            and declaration   = string option
                            and imports       = string option
                            and instantiation = string option
                            and system        = string

                            val noInvariant   = NONE
                            val noSelect      = NONE
                            val noGuard       = NONE
                            val noSync        = NONE
                            val noUpdate      = NONE

                            val noParameter     = NONE
                            val noDeclaration   = NONE
                            val noImports       = NONE
                            val noInstantiation = NONE
                            val noSystem        = ""
                          )

