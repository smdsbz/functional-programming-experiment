fun listToString' [] = ""
  | listToString' [x] = (Int.toString x)
  | listToString' (x::L) = ((Int.toString x) ^ ", " ^ (listToString' L))

fun listToString [] = "[]"
  | listToString L = ("[" ^ (listToString' L) ^ "]")

fun optionToString (opt, toString) = if (Option.isSome opt) then (toString (Option.valOf opt)) else "x"


val _ = (print "\nLab 3 - 1\n")

fun thenAddOne (f, x) = ((f x) + 1)

val _ = (print ("   (thenAddOne (op~, 2)) = " ^ (Int.toString (thenAddOne (op~, 2))) ^ "\n"))


val _ = (print "\nLab 3 - 2\n")

fun mapList (f, []) = []
  | mapList (f, x::L) = (f x)::(mapList (f, L))

val _ = (print ("   (mapList (op~, [1, 2, 3])) = " ^ (listToString (mapList (op~, [1, 2, 3]))) ^ "\n"))


val _ = (print "\nLab 3 - 3\n")

(* Convert @f which receives a single arg to a map version receiving list of args *)
fun mapList' f = fn L => (mapList (f, L))

val _ = (print ("   ((mapList' op~) [1, 2, 3]) = " ^ (listToString ((mapList' op~) [1, 2, 3])) ^ "\n"))


val _ = (print "\nLab 3 - 4\n")

fun findOdd [] = NONE
  | findOdd (x::L) = if (x mod 2) = 1 then (SOME x) else (findOdd L)

val _ = (print ("   (findOdd [2, 2, 2, 1]) = "
                ^ (let val ret = (findOdd [2, 2, 2, 1]) in
                     (optionToString (ret, Int.toString))
                   end) ^ "\n"))


val _ = (print "\nLab 3 - 5\n")

fun sumOfList [] = 0
  | sumOfList [x] = x
  | sumOfList (sum_so_far::x::L) = (sumOfList ((sum_so_far + x)::L))

exception IndexError

(* Takes the @idx-th element from @L, returns nil if @idx not legal index of @L. *)
(* Raises IndexError on illegal @idx. *)
fun takeFromList (L, idx) =
        let val cond = (idx < (List.length L) andalso idx >= 0) in
          if cond
          then ((List.take (L, idx)) @ (List.drop (L, idx + 1)))
          else raise IndexError
        end

(* 'a list * 0 * 'a list -> option *)
(* REQUIRES: @idx called with 0, @evalf of type 'a list -> 'a list option *)
fun forEachTakeAndEval (L, idx, evalf) =
        let
          val taken = (takeFromList (L, idx));
          val result = (evalf taken)
        in
          if (Option.isSome result) then result
          else (forEachTakeAndEval (L, idx + 1, evalf))
        end
        handle IndexError => NONE

(* int list * int -> int list option *)
(* Complexity: [greedy] max O(2^n) *)
fun subsetSumOption (L, s) =
        if (sumOfList L) = s
        then (SOME L)
        else (forEachTakeAndEval
                (L, 0, fn taken => (subsetSumOption (taken, s)))
             )

val _ = (print ("   (subsetSumOption ([1, 2, 3, 4, 5, 6, 7, 8], 24)) = " ^ (optionToString (subsetSumOption ([1, 2, 3, 4, 5, 6, 7, 8], 24), listToString)) ^ "\n"))


val _ = (print "\nLab 3 - 6\n")

(* ('a -> bool) -> 'a list -> bool, where p of type 'a -> bool *)
fun exists p [] = false
  | exists p (x::L) = (p x) orelse (exists p L)

val _ = (print ("   (exists (fn x => x = 10) [1, 2, 3, 10, 11]) = " ^ (Bool.toString (exists (fn x => x = 10) [1, 2, 3, 10, 11])) ^ "\n"))
val _ = (print ("   (exists (fn x => x = 10) []) = " ^ (Bool.toString (exists (fn x => x = 10) [])) ^ "\n"))

fun forall p [] = []
  | forall p (x::L) = (p x)::(forall p L)

val _ = (print ("   (forall op~ [1, 2, 3]) = " ^ (listToString (forall op~ [1, 2, 3])) ^ "\n"))


val _ = (print "\nLab 3 - 7\n")

datatype 'a tree = Null | Node of 'a tree * 'a * 'a tree

fun treeToString_pre (Null, xtostring) = "$"
  | treeToString_pre ((Node (left, x, right)), xtostring) =
        ((treeToString_pre (left, xtostring)) ^ " " ^ (xtostring x) ^ " " ^ (treeToString_pre (right, xtostring)))

(* ('a -> bool) -> 'a tree -> 'a option tree *)
fun treeFilter p Null = Null
  | treeFilter p (Node (left, x, right)) =
        let val neo_x = if (p x) then (SOME x) else NONE in
          (Node ((treeFilter p left), neo_x, (treeFilter p right)))
        end

(*
*       5
*      / `--.
*     2      6
*    / \      \
*   1   3      8
*        \    /
*         4  7
*)
val search_tree = (Node ((Node ((Node (Null, 1, Null)), 2, (Node (Null, 3, (Node (Null, 4, Null)))))), 5, (Node (Null, 6, (Node (Node (Null, 7, Null), 8, Null))))))

val _ = (print ("   val search_tree = (preorder) " ^ (treeToString_pre (search_tree, Int.toString)) ^ "\n"))
val _ = (print ("   (treeFilter (fn x => x < 7 andalso x > 3) search_tree) =\n" ^
                "                     (preorder) " ^
                (treeToString_pre (
                     (treeFilter (fn x => x < 7 andalso x > 3) search_tree),
                     (fn opt => (optionToString (opt, Int.toString)))
                )) ^
                "\n"))


val _ = (print "\n\n")

