//This file was generated from UPPAAL 3.6 Beta 1-pre1 (rev. 1625), Mar 2006

/*
===== Validation Properties:
*/
//NO_QUERY

/*
Gate can receive (and store in queue) msg's from approaching trains.
*/
E<> gate.Occ

/*
Train 0 can reach crossing.
*/
E<> trains(0).Cross

/*
Train 1 can reach crossing.
*/
E<> trains(1).Cross

/*
Train 0 can be crossing bridge while Train 1 is waiting to cross.
*/
E<> trains(0).Cross and trains(1).Stop

/*
Train 0 can cross bridge while the other trains are waiting to cross.
*/
E<> trains(0).Cross and (forall (i : id_t) i != 0 imply trains(i).Stop)

/*
===== Safety Properties:
*/
//NO_QUERY

/*
There is never more than one train crossing the bridge (at
any time instance).
*/
A[] forall (i : id_t) forall (j : id_t) trains(i).Cross && trains(j).Cross imply i == j

/*
There can never be N elements in the queue (thus the array will not overflow).
*/
A[] gate.list[N] == 0

/*
===== Liveness Properties:
*/
//NO_QUERY

/*
Whenever a train approaches the bridge, it will eventually cross.
*/
trains(0).Appr --> trains(0).Cross\
\


/*

*/
trains(1).Appr --> trains(1).Cross

/*

*/
trains(2).Appr --> trains(2).Cross

/*

*/
trains(3).Appr --> trains(3).Cross

/*

*/
trains(4).Appr --> trains(4).Cross

/*

*/
trains(5).Appr --> trains(5).Cross

/*
===== Deadlock checking:
*/
//NO_QUERY

/*
The system is deadlock-free.
*/
A[] not deadlock
