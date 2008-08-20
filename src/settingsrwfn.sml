(* $Id: settings-rw.sml 328 2007-10-31 06:05:10Z tbourke $
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

functor SettingsRWFn (val checkFile : string -> bool) :> SETTINGS_RW =
struct
  val version                    = concat [Version.version, " (",
                                           Version.svnversion, ")"]
  val progName                   = "urpal"

  datatype debug_priority = All | VeryDetailed | Detailed | Outline | NoDebug
  fun debugToInt All          = 0
    | debugToInt VeryDetailed = 1
    | debugToInt Detailed     = 2
    | debugToInt Outline      = 3
    | debugToInt NoDebug      = 4

  fun intToDebug 0 = SOME All
    | intToDebug 1 = SOME VeryDetailed
    | intToDebug 2 = SOME Detailed
    | intToDebug 3 = SOME Outline
    | intToDebug 4 = SOME NoDebug
    | intToDebug _ = NONE

  fun setXOff (_, NONE)                            = ()
    | setXOff (r as ref NONE, SOME v)              = (r := SOME {xoff=v, yoff=0})
    | setXOff (r as ref (SOME {yoff,...}), SOME v) = (r := SOME {xoff=v, yoff=yoff})

  fun setYOff (_, NONE)                            = ()
    | setYOff (r as ref NONE, SOME v)              = (r := SOME {xoff=0,    yoff=v})
    | setYOff (r as ref (SOME {xoff,...}), SOME v) = (r := SOME {xoff=xoff, yoff=v})

  local
    val ref_dtdPath                = ref (NONE : string option)
    val ref_prefix                 = ref (SOME "/usr/local")
    val ref_graphvizPath           = ref (NONE : string option)
    val ref_graphvizEngine         = ref "fdp"
    val ref_maxLabelWidth          = ref 72
    val ref_maxDeclarationWidth    = ref 72
    val ref_newColor               = ref (SOME "#f0e68c")
    val ref_errorColor             = ref (SOME "#f0e68c")
    val ref_urgChanLocColor        = ref (SOME "#7fffd4")

    val ref_splitShiftOld          = ref (NONE : {xoff:int, yoff:int} option)
    val ref_splitShiftNew          = ref (NONE : {xoff:int, yoff:int} option)
    val ref_tabulateShift          = ref (NONE : {xoff:int, yoff:int} option)

    val ref_tabulateLabels         = ref false

    val ref_exitOnFail             = ref false

    val ref_priority = ref NoDebug
  in

  fun dtdPath ()                 = !ref_dtdPath
  fun set_dtdPath nv             = (ref_dtdPath := nv)

  fun prefix ()                  = !ref_prefix

  fun graphvizPath ()            = (case (!ref_graphvizPath, !ref_prefix) of
                                      (NONE, NONE)   => ""
                                    | (NONE, SOME p) => p
                                    | (SOME g,NONE)  => g
                                    | (SOME g,SOME p)=> if OS.Path.isRelative g
                                                        then OS.Path.concat(p,g)
                                                        else g)

  fun set_graphvizPath nv        = (ref_graphvizPath := nv)

  fun set_prefix nv              = (ref_prefix := nv;
                                    case !ref_graphvizPath of
                                      NONE => set_graphvizPath nv
                                    | _    => ())

  fun graphvizEngine ()          = !ref_graphvizEngine
  fun set_graphvizEngine nv      = (ref_graphvizEngine := nv)

  fun maxLabelWidth ()           = !ref_maxLabelWidth
  fun set_maxLabelWidth nv       = (ref_maxLabelWidth := nv)

  fun maxDeclarationWidth ()     = !ref_maxDeclarationWidth
  fun set_maxDeclarationWidth nv = (ref_maxDeclarationWidth := nv)

  fun newColor ()                = !ref_newColor
  fun set_newColor nv            = (ref_newColor := nv)

  fun errorColor ()              = !ref_errorColor
  fun set_errorColor nv          = (ref_errorColor := nv)

  fun urgChanLocColor ()         = !ref_urgChanLocColor
  fun set_urgChanLocColor nv     = (ref_urgChanLocColor := nv)

  fun splitShiftOld ()           = !ref_splitShiftOld
  fun set_splitShiftOld v        = (ref_splitShiftOld := v)
  fun splitShiftNew ()           = !ref_splitShiftNew
  fun set_splitShiftNew v        = (ref_splitShiftNew := v)

  fun set_splitShiftOldX v       = setXOff (ref_splitShiftOld, v)
  fun set_splitShiftOldY v       = setYOff (ref_splitShiftOld, v)
  fun set_splitShiftNewX v       = setXOff (ref_splitShiftNew, v)
  fun set_splitShiftNewY v       = setYOff (ref_splitShiftNew, v)

  fun set_tabulateShiftX v       = setXOff (ref_tabulateShift, v)
  fun set_tabulateShiftY v       = setYOff (ref_tabulateShift, v)

  fun tabulateShift ()           = case !ref_tabulateShift of
                                     NONE   => {xoff=10, yoff=10}
                                   | SOME p => p
  fun set_tabulateShift v        = (ref_tabulateShift := v)

  fun tabulateLabels ()          = !ref_tabulateLabels
  fun set_tabulateLabels v       = (ref_tabulateLabels := v)

  fun exitOnFail ()              = !ref_exitOnFail
  fun set_exitOnFail v           = (ref_exitOnFail := v)

  fun set_priority d = ignore(Option.map(fn v=>ref_priority:=v) (intToDebug d))
  fun showDebug (d1) = debugToInt d1 >= debugToInt (!ref_priority)
  fun priority () = !ref_priority

  end (* local *)

  fun warn msg = (TextIO.output (TextIO.stdErr,
                                 String.concat (progName::":"::msg));
                  TextIO.output (TextIO.stdErr, "\n"))
  
  fun validate () = let

      fun check test (name, path) = let
          val r = test path handle SysErr => false
          val _ = if r then ()
                  else warn ["setting ", name, " (", path ,") is invalid."]
        in r end

      fun checkoFile (_, NONE)   = true
        | checkoFile (s, SOME p) = check checkFile (s, p)
      
      fun addFile (f, d) = OS.Path.joinDirFile {dir=d, file=f}

      val results = [ checkoFile ("dtd_path", dtdPath ()),
                      check checkFile ("graphviz/path",
                                      foldl addFile (graphvizPath ())
                                            ["bin", "dot"])
                    ]
    in List.all (fn x=>x) results end


  local
    structure CT = ConfigTree

    val <*< = Option.compose; infixr <*<
    val <**< = Option.composePartial; infixr <**<

    fun some x = SOME (SOME x)
  in
  fun loadConfigFile rdr strm = let
      val cfgFile = CT.parse rdr strm
                    handle CT.ParseError l => [] before
                      warn ["error at line ", Int.toString l,
                            "of config file (all contents ignored)."]
      fun updateOption (path, changeSetting, f) = ignore (Option.map
                                        changeSetting (f (map Atom.atom path)))

      fun % f = fn v=> f (cfgFile, v)
    in
      List.app updateOption
      [(["dtd_path"],set_dtdPath, some <**< %CT.getString),
       (["prefix"],  set_prefix,  some <**< %CT.getString)];

      List.app updateOption
      [(["max_label_width"],       set_maxLabelWidth,       %CT.getInt),
       (["max_declaration_width"], set_maxDeclarationWidth, %CT.getInt)];

      updateOption (["graphviz", "engine"], set_graphvizEngine, %CT.getString);
      updateOption (["graphviz", "path"], set_graphvizPath,
                                                    some <**< %CT.getString);

      updateOption (["split_shift_old", "x"],       set_splitShiftOldX,
                                                    some <**<    %CT.getInt);
      updateOption (["split_shift_old", "y"],       set_splitShiftOldY,
                                                    some <**<    %CT.getInt);
      updateOption (["split_shift_new", "x"],       set_splitShiftNewX,
                                                    some <**<    %CT.getInt);
      updateOption (["split_shift_new", "y"],       set_splitShiftNewY,
                                                    some <**<    %CT.getInt);
      updateOption (["tabulate_shift", "x"],        set_tabulateShiftX,
                                                    some <**<    %CT.getInt);
      updateOption (["tabulate_shift", "y"],        set_tabulateShiftY,
                                                    some <**<    %CT.getInt);

      List.app updateOption
      [(["new_color"],           set_newColor,       some <**< %CT.getString),
       (["error_color"],         set_errorColor,     some <**< %CT.getString),
       (["urgent_chanloc_color"],set_urgChanLocColor,some <**< %CT.getString)];

      updateOption (["exit_on_fail"], set_exitOnFail,          %CT.getBool);
      updateOption (["debug"],   set_priority,                 %CT.getInt)
    end

  fun saveConfigFile outs = let

      val % = Atom.atom
      fun id x = x

      fun optEntry (n, w, v) = Option.map (fn v=>CT.Entry (n, w v)) v
      fun defEntry (n, w, v) = SOME (CT.Entry (n, w v))

      fun optOffset (n, NONE) = NONE
        | optOffset (n, SOME {xoff, yoff}) = SOME (CT.Section (n,
            [CT.Entry (%"x", CT.Int xoff), CT.Entry (%"y", CT.Int yoff)]))

      val cfg = List.mapPartial id [
          optEntry (%"dtd_path",            CT.String,dtdPath()),

          defEntry (%"max_label_width",      CT.Int, maxLabelWidth()),
          defEntry (%"max_declaration_width",CT.Int, maxDeclarationWidth()),

          optEntry (%"new_color",           CT.Color, newColor()),
          optEntry (%"error_color",         CT.Color, errorColor()),
          optEntry (%"urgent_chanloc_color",CT.Color, urgChanLocColor()),

          optOffset (%"split_shift_old",    splitShiftOld ()),
          optOffset (%"split_shift_new",      splitShiftNew      ()),
          optOffset (%"tabulate_shift",       SOME (tabulateShift ())),

          defEntry (%"exit_on_fail",        CT.Bool, exitOnFail ()),

          SOME (CT.Section (%"graphviz", [
            CT.Entry (%"path",   CT.String (graphvizPath ())),
            CT.Entry (%"engine", CT.String (graphvizEngine ()))])),

          defEntry (%"debug",             CT.Int, debugToInt (priority ()))
        ]
    in CT.output (outs, cfg) end

  end (* local *)

end

