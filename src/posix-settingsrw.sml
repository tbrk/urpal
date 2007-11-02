(* $Id: settings-rw.sml 328 2007-10-31 06:05:10Z tbourke $ *)

structure SettingsRW=SettingsRWFn (
  val checkFile = Posix.FileSys.ST.isReg o Posix.FileSys.stat
)

