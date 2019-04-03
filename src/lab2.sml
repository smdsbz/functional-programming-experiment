fun listToString' [] = ""
  | listToString' [x] = (Int.toString x)
  | listToString' (x::L) = ((Int.toString x) ^ ", " ^ (listToString' L))

fun listToString [] = "[]"
  | listToString L = ("[" ^ (listToString' L) ^ "]")


val _ = (print "\nLab 2 - 1\n")

fun reverse [] = []
  | reverse (left::L) = (reverse L)@[left]  (* @ operator cause list copy, O(n) *)

val _ = (print ("   (reverse [1, 2, 3, 4]) = " ^ (listToString (reverse [1, 2, 3, 4])) ^ "\n"))
val _ = (print ("   (reverse [1, 2, 3]) = " ^ (listToString (reverse [1, 2, 3])) ^ "\n"))

(* Implemented with two stacks - one for input and one for output *)
fun reverse_helper ([], out_stack) = out_stack
  | reverse_helper (x::in_stack, out_stack) = reverse_helper(in_stack, x::out_stack)
fun reverse' L = (reverse_helper (L, []))

val _ = (print ("   (reverse' [1, 2, 3]) = " ^ (listToString (reverse' [1, 2, 3])) ^ "\n"))


val _ = (print "\nLab 2 - 2\n")

fun interleave ([], other) = other
  | interleave (first, []) = first
  | interleave (x::first, y::second) = x::y::(interleave (first, second))

val _ = (print ("   (interleave ([1, 2, 3], [5, 6, 7])) = " ^ (listToString (interleave ([1, 2, 3], [5, 6, 7]))) ^ "\n"))
val _ = (print ("   (interleave ([1, 2, 3, 4], [5, 6])) = " ^ (listToString (interleave ([1, 2, 3, 4], [5, 6]))) ^ "\n"))


val _ = (print "\nLab 2 - 3\n")

datatype tree = Empty
              | Node of tree * int * tree   (* left * value * right *)

fun fullPreTrav Empty = "$ "
  | fullPreTrav (Node (left, x, right)) = ((Int.toString x) ^ " " ^ (fullPreTrav left) ^ (fullPreTrav right))

fun treeToList_in Empty = []
  | treeToList_in (Node (left, x, right)) = ((treeToList_in left) @ x :: (treeToList_in right))

exception EmptyList

(* Extracts the item in the middle of the list @L, and returns the splited lists *)
fun split [] = raise EmptyList
  | split L = let val half = ((List.length L) div 2) in
                ((List.take (L, half)), (List.nth (L, half)), (List.drop (L, (half + 1))))
              end

fun listToTree [] = Empty
  | listToTree L = let val (left, mid, right) = (split L) in
                     (Node ((listToTree left), mid, (listToTree right)))
                   end

(*
*       3
*      / \
*     2  5
*    /  /
*   1  4
*)
val tmp_tree = (listToTree [1, 2, 3, 4, 5])
val _ = (print ("   tmp_tree = (listToTree [1, 2, 3, 4, 5]) = (preorder) " ^ (fullPreTrav tmp_tree) ^ "\n"))


val _ = (print "\nLab 2 - 4\n")

fun revT Empty = Empty
  | revT (Node (left, x, right)) = (Node ((revT right), x, (revT left)))

(*
*       3
*      / \
*     5   2
*      \   \
*       4   1
*)
val tmp_tree_rev = (revT tmp_tree)
val _ = (print ("   tmp_tree_rev = (revT tmp_tree) = (preorder) " ^ (fullPreTrav tmp_tree_rev) ^ "\n"))
val _ = (print ("   ((treeToList_in tmp_tree_rev) = (reverse (treeToList_in tmp_tree))) = " ^ (Bool.toString ((treeToList_in tmp_tree_rev) = (reverse (treeToList_in tmp_tree)))) ^ "\n"))


val _ = (print "\nLab 2 - 5\n")

fun binarySearch (Empty, _) = false
  | binarySearch ((Node (left, x, right)), y) =
        let val rst = (Int.compare (y, x)) in
          if rst = LESS
          then (binarySearch (left, y))
          else if rst = EQUAL
          then true
          else (binarySearch (right, y))
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
val search_tree = (Node ((Node ((Node (Empty, 1, Empty)), 2, (Node (Empty, 3, (Node (Empty, 4, Empty)))))), 5, (Node (Empty, 6, (Node (Node (Empty, 7, Empty), 8, Empty))))))

val _ = (print ("   search_tree = (preorder) " ^ (fullPreTrav search_tree) ^ "\n"))
val _ = (print ("   (binarySearch (search_tree, 7)) = " ^ (Bool.toString (binarySearch (search_tree, 7))) ^ "\n"))
val _ = (print ("   (binarySearch (search_tree, 12)) = " ^ (Bool.toString (binarySearch (search_tree, 12))) ^ "\n"))


val _ = (print "\n\n")

