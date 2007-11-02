(* $Id$

   Based on devices in the SML/NJ PP library.
 *)

structure PPDevString : sig

    include PP_DEVICE

    val openDev : {dst: string, wid : int} -> device
    val toString : device -> string

  end
=
struct

  datatype device = DEV of {
      dst : string list ref,
      wid : int
    }

  (* no style support *)
  type style = unit
  fun sameStyle _ = true
  fun pushStyle _ = ()
  fun popStyle _ = ()
  fun defaultStyle _ = ()

  fun openDev {dst, wid} = DEV {dst=ref [dst], wid=wid}
  fun toString (DEV {dst, ...}) = String.concat (rev (!dst))

  (* maximum printing depth (in terms of boxes) *)
  fun depth _ = NONE

  (* the width of the device *)
  fun lineWidth (DEV{wid, ...}) = SOME wid
  (* the suggested maximum width of text on a line *)
  fun textWidth _ = NONE

  fun add (DEV{dst, ...}, str) = (dst := str :: (!dst))

  fun space (dev, n) = add (dev, StringCvt.padLeft #" " n "")
  fun newline dev = add (dev, "\n")

  fun string (dev, s) = add (dev, s)
  fun char (dev, c) = add (dev, Char.toString c)

  fun flush _ = ()

end;

