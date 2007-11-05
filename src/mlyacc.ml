local
val root = "/usr/local/lib/mlton/sml"
val mlyacc = [
"base.sig",
"join.sml",
"lrtable.sml",
"stream.sml",
"parser2.sml",
""]
in
val _ = List.app (fn"" => () | s => use(root^"/mlyacc-lib/"^s)) mlyacc
end;
