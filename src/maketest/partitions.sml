(* $Id$

   Generate all partitions for a set of given size.
   The size of the result grows very quickly as n increases (Bell's Number).

 *)

structure Partitions =
struct
  type 'a class = 'a list
  type 'a partition = 'a class list

  fun addToPartition (n, base, [] : 'a partition) =
        [[n] :: base] : 'a partition list
    | addToPartition (n, base, c::cs) = 
        (base @ ((n::c)::cs)) :: (addToPartition (n, c::base, cs))

  fun addToPartitions (n, ps : 'a partition list) =
    List.concat (map (fn p=> addToPartition (n, [], p)) ps)

  (*           partition   equivalence class
                        \  |
          partition list \ |
                        \ ||
                         |||           *)
  fun makeList [x]     = [[[x]]]
    | makeList (x::xs) = addToPartitions (x, makeList xs)
    | makeList []      = [[[]]]
end

