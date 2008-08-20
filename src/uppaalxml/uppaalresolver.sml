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

structure UppaalResolver : Resolve =
struct
  exception DtdNotSupported of string
  val publicID="-//Uppaal Team//DTD Flat System 1.1//EN"

  local
  fun trySystem (pubstr, NONE)            = raise DtdNotSupported pubstr
    | trySystem (_, SOME (base, file, _)) = Uri.uriJoin (base, file)
  in

  fun resolveExtId (Base.EXTID(SOME (str, _), sys)) =
      if str=publicID then case (Settings.dtdPath ()) of
                             NONE   => trySystem (str, sys)
                           | SOME s => Uri.String2Uri s
      else trySystem (str, sys)

    | resolveExtId (Base.EXTID(NONE, sys)) = trySystem ("-none given-", sys)

  end (* local *)
      
end

