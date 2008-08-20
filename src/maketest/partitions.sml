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
 *
 * Generate all partitions for a set of given size.
 * The size of the result grows very quickly as n increases (Bell's Number).
 *
 * TODO: * consider a lazy version (return partitions one-by-one)
 *       * make a functor that can allows the partition predictates
 *         for transitions to be created with the partitions.
 *
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

