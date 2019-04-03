val _ = (print "\nLab 1 - 1\n")

(* sum : int list -> int *)
(* REQUIRES: true *)
(* ENSURES: sum(L) evaluates to the sum of the integers in L. *)
fun sum [] = 0
  | sum (x::L) = x + (sum L)

val _ = (print ("   (sum [1, 2, 3]) = " ^ (Int.toString (sum [1, 2, 3])) ^ "\n"))


val _ = (print "\nLab 1 - 2\n")

(* mult : int list -> int *)
(* REQUIRES: true *)
(* ENSURES: mult(L) evaluates to the product of the integers in L. *)
fun mult [] = 1
  | mult (x::L) = x * (mult L)

val _ = (print ("   (mult [1, 2, 3]) = " ^ (Int.toString (mult [1, 2, 3])) ^ "\n"))


val _ = (print "\nLab 1 - 3\n")

(* Mult : int list list -> int *)
(* REQUIRES: true *)
(* ENSURES: mult(R) evaluates to the product of all the integers in the lists of R. *)
fun Mult [] = 1
  | Mult (r::R) = (mult r) * (Mult R)

val _ = (print ("   (Mult [[1], [2, 3]]) = " ^ (Int.toString (Mult [[1], [2, 3]])) ^ "\n"))


val _ = (print "\nLab 1 - 4\n")

(* mult' : int list * int -> int *)
(* REQUIRES: called with a = 1. *)
(* ENSURES: mult’(L, a) similar to mult, but introduced tail recursion. *)
fun mult' ([ ], a) = a
  | mult' (x :: L, a) = mult' (L, x * a)

val _ = (print ("   (mult' ([1, 2, 3], 1)) = " ^ (Int.toString (mult' ([1, 2, 3], 1))) ^ "\n"))

fun Mult' ([], a) = a
  | Mult' (l::L, a) = (Mult' (L, (mult' (l, 1)) * a))

val _ = (print ("   (Mult' ([[1], [2, 3]], 1)) = " ^ (Int.toString (Mult' ([[1], [2, 3]], 1))) ^ "\n"))


val _ = (print "\nLab 1 - 5\n")

(* double : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n. *)
fun double (0 : int) : int = 0
  | double n = 2 + double (n - 1)

(* NOTE: Mind the priorities, `(double 3 - 1)` evals to `((double 3) - 1)`! *)
fun square 0 = 0
  | square 1 = 1
  | square 2 = 4
  | square n = if n < 0
               then (square (~n))
               else if (n mod 2) = 0
                    then (double (double (square (n div 2))))
                    else (square (n - 1)) + (double (n - 1)) + 1

val _ = (print ("   (square 6) = " ^ (Int.toString (square 6)) ^ "\n"))
val _ = (print ("   (square ~8) = " ^ (Int.toString (square ~8)) ^ "\n"))


val _ = (print ("\nLab 1 - 6\n"))

(* divisibleByThree : int -> bool *)
(* REQUIRES: true *)
(* ENSURES: divisibleByThree n evaluates to true if n is a multiple of 3 and to false otherwise *)
fun divisibleByThree 0 = true
  | divisibleByThree 1 = false
  | divisibleByThree 2 = false
  | divisibleByThree n = if n < 0 then (divisibleByThree (n + 3)) else (divisibleByThree (n - 3))

val _ = (print ("   (divisibleByThree 99) = " ^ (Bool.toString (divisibleByThree 99)) ^ "\n"))
val _ = (print ("   (divisibleByThree ~102) = " ^ (Bool.toString (divisibleByThree ~102)) ^ "\n"))


val _ = (print "\nLab 1 - 7\n")

(* evenP : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: evenP n evaluates to true iff n is even. *)
fun evenP (0 : int) : bool = true
  | evenP 1 = false
  | evenP n = evenP (n - 2)

fun oddP 0 = false
  | oddP 1 = true
  | oddP n = if n < 0 then (oddP (n + 2)) else (oddP (n - 2))

val _ = (print ("   (oddP ~9) = " ^ (Bool.toString (oddP ~9)) ^ "\n"))
val _ = (print ("   (oddP 10) = " ^ (Bool.toString (oddP 10)) ^ "\n"))


val _ = (print "\n\n")

