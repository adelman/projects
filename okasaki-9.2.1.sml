(* Matt Adelman
 * Okaskai 9.2.1
 * Binary Random Access Lists.
 *)

(* The BinaryRandomAccessList structure from the book. *)
structure BinaryRandomAccessList = 
struct

  (* The 'a Tree datatype from Okasaki *)
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  (* The Digit datatype from Okasaki *)
  datatype 'a Digit = ZERO | ONE of 'a Tree
  (* The 'a RList type as defined in the signature *)
  type 'a RList = 'a Digit list

  (* Exceptions for head/tail and lookup/update *)
  exception Empty
  exception Subscript

  (* The Empty list *)
  val empty = []
  (* If a list is empty, then the RList is empty *)
  fun isEmpty ts = null ts

  (* size ts : 'a Tree = 1 if ts is a leaf, otherwise 
   * size NODE (w, t1, t2) : 'a Tree = w : int *)
  fun size (LEAF x) = 1
    | size (NODE (w, t1, t2)) = w

  (* link (t1 : 'a Tree, t2 : 'a Tree) = t : Tree
   * Where size t = size t1 + size t2, the left tree is t1, and the right tree
   * is t2.
   *)
  fun link(t1, t2) = NODE(size t1 + size t2, t1, t2)

  (* consTree (t : 'a Tree, ts : 'a RList) = rs : 'a RList
   * Where rs is an 'a RList containing the tree t and the trees in ts.
   *) 
  fun consTree (t, []) = [ONE t]
    | consTree (t, ZERO :: ts) = ONE t :: ts
    | consTree (t1, ONE t2 :: ts) = ZERO :: consTree (link (t1, t2), ts)

  (* unconsTree rs : 'a RList = (t : 'a Tree, rs' : 'a RList)
   * Where t is the first tree in rs and rs' is the rest of the 'a RList.
   *)
  fun unconsTree [] = raise Empty
    | unconsTree [ONE t] = (t, [])
    | unconsTree (ONE t :: ts) = (t, ZERO :: ts)
    | unconsTree (ZERO :: ts) = 
      let val (NODE (_, t1, t2), ts') = unconsTree ts
      in (t1, ONE t2 :: ts') end

  (* cons (x : 'a, ts : 'a RList) = rs 'a RList
   * Where rs is a binary random access list and the first element of rs is now
   * x.
   *)
  fun cons (x, ts) = consTree (LEAF x, ts)
  
  (* head rs : 'a RList = x : 'a where x is the first element in rs. *)
  fun head ts = let val (LEAF x, _) = unconsTree ts in x end

  (* tail rs : 'a RList = rs' : 'a RList where rs' is a binary random access
   * list and rs' consists of all the elements of rs except the first.
   *)
  fun tail ts = let val (_, ts') = unconsTree ts in ts' end

  local
    (* ensure_list (ds : 'a Digit list, n : int) -> rs : 'a RList
     * Where ds is what we will call an "increasing Digit list" where if there
     * are r leading zeros then the size of the first tree is at least n * 2^r.
     * We then have that rs is an RList containing all of the elements of ds.
     *)
    fun ensure_rlist ([], n) = []
      | ensure_rlist (ZERO :: xs, n) = ZERO :: ensure_rlist(xs, n * 2)
      | ensure_rlist (ONE x :: xs, n) = 
      if size x = n then ONE x :: ensure_rlist(xs, n * 2)
      else ZERO :: ensure_rlist (ONE x :: xs, n * 2)

    (* dropTree (n : int, t : tree) = ds : 'a Digit list
     * Where rs is a digit list consisting of the last w - n elements of t, 
     * where t is a leaf complete binary tree.
     *)
    fun dropTree (n, t) =
    let 
      (* dropTree' (n : int, ts : 'a Digit list, t : 'a Tree) = 
       * ts' : 'a Digit list 
       * Where ts' is an 'a Digit list and the elements of ts' are the last
       * w - n elements of t as an 'a Digit list @ ts, and w is the size of t.
       *)
      fun dropTree' (0, t, ts) = ONE t :: ts
        | dropTree' (1, LEAF x, ts) = ts
        | dropTree' (n, LEAF x, ts) = raise Subscript
        | dropTree' (n, NODE(w, t1, t2), ts) = 
        if n < w div 2 then dropTree'(n, t1, ONE t2 :: ts)
        else dropTree'(n - w div 2, t2, ts)
    in
      dropTree' (n, t, [])
    end

    (* drop' (n : int, rs : 'a RList) = ds : 'a Digit list
     * Where ds is an 'a Digit list consisting of the last w - n elements of 
     * ds where w is the size of rs. 
     *)
    fun drop' (0, ts) = ts
      | drop' (n, []) = raise Subscript
      | drop' (n, ZERO :: ts) = drop' (n, ts)
      | drop' (n, ONE t :: ts) = 
      if n < size t then dropTree (n, t) @ ts 
      else drop' (n - size t, ts)

  in
    (* drop (n : int, rs : 'a RList) = rs' : 'a RList
     * Where if w is the number of elements in rs then rs' consists of the last
     * w - n elements of rs.
     *)
    fun drop (n, ts) = ensure_rlist (drop'(n, ts), 1)
  end

  (* lookupTree (i : int, t : 'a Tree) = x : 'a
   * Where x is the ith element in tree t where since t is a leaf complete
   * binary tree, x is the ith leaf from the left, starting at 0.
   *)
  fun lookupTree (0, LEAF x) = x
    | lookupTree (i, LEAF x) = raise Subscript
    | lookupTree (i, NODE(w, t1, t2)) = 
      if i < w div 2 then lookupTree (i, t1)
      else lookupTree (i - w div 2, t2)

  (* updateTree (i : int, x : 'a, t : 'a Tree = t' : 'a Tree
   * Where the elements of t' are the same as t, except the element in the ith
   * leaf has been replaced by x.
   *)
  fun updateTree (0, y, LEAF x) = LEAF y
    | updateTree (i, y, LEAF x) = raise Subscript
    | updateTree (i, y, NODE (w, t1, t2)) = 
      if i < w div 2 then NODE(w, updateTree (i, y, t1), t2)
      else NODE(w, t1, updateTree (i - w div 2, y, t2))

  (* lookup (i : int, rs : 'a RList) = x : 'a
   * Where x is the ith element in rs, a binary random access list.
   *)
  fun lookup (i, []) = raise Subscript
    | lookup (i, ZERO :: ts) = lookup (i, ts)
    | lookup (i, ONE t :: ts) = 
      if i < size t then lookupTree (i, t) else lookup (i - size t, ts)

  (* update (i : int, x : 'a, rs : 'a RList) = rs' : 'a RList
   * Where the element in the ith index is now x.
   *)
  fun update (i, y, []) = raise Subscript
    | update (i, y, ZERO :: ts) = ZERO :: update (i, y, ts)
    | update (i, y, ONE t :: ts) = 
      if i < size t then ONE (updateTree (i, y, t)) :: ts
      else ONE t :: update (i - size t, y, ts)

  (* create (n : int, x : 'a) = rs : 'a RList
   * Where rs consists of n copies of x.
   *)
  fun create(n, x) = 
  let
    (* create' (n : int, x : 'a, t : 'a Tree) = rs 'a RList
     * Where the keys of rs are n copies of x, using the tree t to make
     * sure the function runs in O(lg n) time.
     *)
    fun create' (0, x, t) = []
      | create' (n, x, t) = 
        if n mod 2 = 1 then 
        ONE t :: create'(n div 2, x, NODE(size t + size t, t, t))
        else ZERO :: create'(n div 2, x,  NODE(size t + size t, t, t))
  in
    create'(n, x, LEAF x)
  end
  
end

(* A sparse implementation of Binary Random Access Lists *)
structure SparseBinaryRandomAccessList = 
struct

  (* The 'a Tree datatype from Okasaki *)
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  (* The 'a RList type as defined in the signature *)
  type 'a RList = 'a Tree list

  (* Exceptions for head/tail and lookup/update *)
  exception Empty
  exception Subscript

  (* The Empty list *)
  val empty = []
  (* If a list is empty, then the RList is empty *)
  fun isEmpty ts = null ts

  (* size ts : 'a Tree = 1 if ts is a leaf, otherwise 
  * size NODE (w, t1, t2) : 'a Tree = w : int *)
  fun size (LEAF x) = 1
    | size (NODE (w, t1, t2)) = w

  (* link (t1 : 'a Tree, t2 : 'a Tree) = t : Tree
   * Where size 1 = size t1 + size t2, the left tree is t1, and the right tree
   * is t2.
   *)
  fun link(t1, t2) = NODE(size t1 + size t2, t1, t2)

  (* consTree (t : 'a Tree, ts : 'a RList) = rs : 'a RList
   * Where rs is an 'a RList containing the tree t and the trees in ts.
   *) 
  fun consTree (t, []) = [t]
    | consTree (t, t' :: ts) = 
    if size t < size t' then t :: t' :: ts
    else if size t > size t' then t' :: consTree(t, ts)
    else consTree(link(t, t'), ts)

  (* cons (x : 'a, ts : 'a RList) = rs 'a RList
   * Where rs is a binary random access list and the first element of rs is now
   * x.
   *)
  fun cons (x, ts) = consTree (LEAF x, ts)
  
  (* head rs : 'a RList = x : 'a where x is the first element in rs. 
   * I could not figure out a clever way to incorporate unconsTree, so I wrote
   * new head and tail functions.
   * *)
  fun head [] = raise Empty
    | head (LEAF x :: ts) = x
    | head (NODE(_, t1, t2) :: ts) = head(t1 :: ts)

  (* tail rs : 'a RList = rs' : 'a RList where rs' is a binary random access
   * list and rs' consists of all the elements of rs except the first.
   *)
  fun tail [] = raise Empty
    | tail (t :: ts) = 
    let
      fun tail' (LEAF x, ts) = ts
        | tail' (NODE(w, t1, t2), ts) = tail'(t1, t2 :: ts)
    in
      tail'(t, ts)
    end
    
  (* lookupTree (i : int, t : 'a Tree) = x : 'a
   * Where x is the ith element in tree t where since t is a leaf complete
   * binary tree, x is the ith leaf from the left, starting at 0.
   *)
  fun lookupTree (0, LEAF x) = x
    | lookupTree (i, LEAF x) = raise Subscript
    | lookupTree (i, NODE(w, t1, t2)) = 
      if i < w div 2 then lookupTree (i, t1)
      else lookupTree (i - w div 2, t2)

  (* updateTree (i : int, x : 'a, t : 'a Tree = t' : 'a Tree
   * Where the elements of t' are the same as t, except the element in the ith
   * leaf has been replaced by x.
   *)
  fun updateTree (0, y, LEAF x) = LEAF y
    | updateTree (i, y, LEAF x) = raise Subscript
    | updateTree (i, y, NODE (w, t1, t2)) = 
      if i < w div 2 then NODE(w, updateTree (i, y, t1), t2)
      else NODE(w, t1, updateTree (i - w div 2, y, t2))

  (* lookup (i : int, rs : 'a RList) = x : 'a
   * Where x is the ith element in rs, a binary random access list.
   *)
  fun lookup (i, []) = raise Subscript
    | lookup (i, t :: ts) = 
      if i < size t then lookupTree (i, t) else lookup (i - size t, ts)

  (* update (i : int, x : 'a, rs : 'a RList) = rs' : 'a RList
   * Where the element in the ith index is now x.
   *)
  fun update (i, y, []) = raise Subscript
    | update (i, y, t :: ts) = 
      if i < size t then (updateTree (i, y, t)) :: ts
      else t :: update (i - size t, y, ts)

end
