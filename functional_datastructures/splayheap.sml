(* Matt Adelman
 * Comp 401
 *)

(* The SplayHeap functor from the book with an added sort function. *)
functor SplayHeap(Element: ORDERED) : HEAP =
struct

  structure Elem = Element

  datatype Heap = E | T of Heap * Elem.T * Heap

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  (* partition(x : Elem.T, h : Heap) = (h1 : Heap, h2 : Heap)
   * Where h1 and h2 are both splay heaps, for all keys a in h1, a <= x, and for
   * all keys b in h2, b > x. We have the precondition that h is a splay heap.
   *)
  fun partition (pivot, E) = (E,E)
    | partition (pivot, t as T(a,x,b)) = 
    if Elem.leq(x, pivot) then
      case b of
           E => (t, E)
         | T(b1, y, b2) =>
             if Elem.leq(y, pivot) then
               let val (small, big) = partition (pivot, b2)
               in (T(T(a,x,b1), y, small), big) end
             else
               let val (small, big) = partition (pivot, b1)
               in (T(a, x, small), T(big, y, b2)) end
    else
      case a of
           E => (E, t)
         | T(a1, y, a2) =>
             if Elem.leq (y, pivot) then
               let val (small, big) = partition (pivot, a2)
               in (T(a1, y, small), T(big, x, b)) end
             else
               let val (small, big) = partition (pivot, a1)
               in (small, T(big, y, T(a2, x, b))) end

  (* insert(x : Elem.T, h : Heap) = h' : Heap 
   * Where h' is a splay heap and keys(h') = keys(h) U {x}, with the possibility
   * of having duplicate elements. We have the precondition that h is a splay
   * heap.
   *)
  fun insert(x, t) = let val (a, b) = partition(x, t) in T(a,x,b) end

  (* merge(h1 : Heap, h2 : Heap) = h : Heap
   * Where h is a splay heap and keys(h) = keys(h1) U keys(h2) with the
   * possibility of duplicates. We have the precondition that both h1 and h2 are
   * splay heaps.
   *)
  fun merge (E, t) = t
    | merge (T(a, x, b), t) = 
    let val (ta, tb) = partition (x, t)
    in T(merge (ta, a), x, merge(tb, b)) end

  (* findMin h : Heap = x : Elem.T
   * Where x <= all of the keys(h). We have the precondition that h is a splay
   * heap.
   *)
  fun findMin E = raise Empty
    | findMin (T(E, x, b)) = x
    | findMin (T(a, y, x)) = findMin a

  (* deleteMin h : Heap = h' : Heap
   * Where h' is a splay heap and keys(h') = keys(h) \ {x}, where x is the
   * minimum element in h. We have the precondition that ts is a splay heap.
   *)
  fun deleteMin E = raise Empty
    | deleteMin (T(E, x, b)) = b
    | deleteMin (T(T(E, x, b), y, c)) = T(b, y, c)
    | deleteMin (T(T(a, x, b), y, c)) = T(deleteMin a, x, T(b, y, c))


  (* Helper functions for our sort. *)
  local
    (* inOrder h : Heap = xs : Elem.T list
     * Where xs is a list of the elements of heap h in in-order traversal.
     * We have the precondition that h is a splay heap. Note that the in-order
     * traversal of a splay heap is a non-decreasing list of the elements in the
     * heap, by the BST properties.
     *)
    fun inOrder E = []
      | inOrder t = 
      let
        (* Note that this auxiliary function runs in O(n) time because we only
         * perform one :: operation per node, and then one reverse at the very
         * end. Therefore we have 2n :: operations, so the function is O(n).
         *)
        fun inOrder' (E, ys) = ys
          | inOrder' (T(a, y, b), ys) = 
          let
            val ys'  = inOrder'(a, ys)
            val ys'' = inOrder'(b, y :: ys')
          in
            ys''
          end
      in
        rev (inOrder' (t, []))
      end

    (* insertFromList xs : Elem.T list = h : Heap
     * Where keys(h) = keys(xs). Since insert is worst case O(lg n) time, this
     * function is worst case O(n lg n).
     *)
    fun insertFromList [] = E
      | insertFromList (x :: xs) = 
      let
        val t = insertFromList xs
      in
        insert(x, t)
      end
  in
    (* sort xs : Elem.T list = xs' : Elem.T list where keys(xs') = keys(xs) 
     * sorted in non-decreasing order.
     *)
    fun sort xs = inOrder (insertFromList xs)
  end

end
