(* Digits using 1s and 2s *)
structure Zeroless = 
struct

  datatype Digit = ONE | TWO
  type Nat = Digit list

  exception Negative

  (* inc n : Nat = n' : Nat, where the value of n' = n + 1. *)
  fun inc [] = [ONE]
    | inc (ONE :: ds) = TWO :: ds
    | inc (TWO :: ds) = ONE :: inc ds

  (* dec n : Nat = n' : Nat, where the value of n' = n - 1
   * If n = [], then we raise the exception Negative.
   *)
  fun dec [] = raise Negative 
    | dec [ONE] = []
    | dec (TWO :: ds) = ONE :: ds
    | dec (ONE :: ds) = TWO :: dec ds

  (* add (ns : Nat, ms : Nat) = ns', where the value of ns' = ns + ms. *)
  fun add (ws, []) = ws
    | add ([], ws) = ws
    | add (ONE :: ds, ONE :: ds') = TWO :: add (ds, ds')
    | add (TWO :: ds, ONE :: ds') = ONE :: inc (add (ds, ds'))
    | add (ONE :: ds, TWO :: ds') = ONE :: inc (add (ds, ds'))
    | add (TWO :: ds, TWO :: ds') = TWO :: inc (add (ds, ds'))

end

(* Zeroless representation of binary random access lists. *)
structure ZerolessBinaryRandomAccessList = 
struct

  (* Tree datatype for storing data *)
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  (* Digits to make up the digit list *)
  datatype 'a Digit = ONE of 'a Tree | TWO of 'a Tree * 'a Tree
  (* A Zeroless representation of a Binary Random Access List *)
  type 'a RList = 'a Digit list

  exception Empty
  exception Subscript

  (* The empty RList *)
  val empty = []
  (* If the list is null, there are no elements in the RList. *)
  fun isEmpty rs = null rs

  (* size ts : 'a Tree = 1 if ts is a leaf, otherwise 
   * size NODE (w, t1, t2) : 'a Tree = w : int *)
  fun size (LEAF x) = 1
    | size (NODE (x, _, _)) = x

  (* link (t1 : 'a Tree, t2 : 'a Tree) = t : Tree
   * Where size t = size t1 + size t2, the left tree is t1, and the right tree
   * is t2.
   *)
  fun link (t1, t2) = NODE(size t1 + size t2, t1, t2)

  (* head rs : 'a RList = x : 'a where x is the first element in rs. *)
  fun head [] = raise Empty
    | head (ONE(LEAF x) :: _) = x
    | head (TWO(LEAF x, LEAF y) :: _) = x
    | head _ = raise Subscript

  (* tail rs : 'a RList = rs' : 'a RList where rs' is a binary random access
   * list and rs' consists of all the elements of rs except the first.
   *)
  fun tail [] = raise Empty
    | tail (ONE(_) :: rs) = rs
    | tail (TWO(LEAF _, LEAF x) :: rs) = ONE(LEAF x) :: rs
    | tail _ = raise Subscript

  (* consTree (t : 'a Tree, ts : 'a RList) = rs : 'a RList
   * Where rs is an 'a RList containing the tree t and the trees in ts.
   *) 
  fun consTree (t, []) = [ONE t]
    | consTree (t, ONE t' :: rs) = TWO(t, t') :: rs
    | consTree (t, TWO(t1, t2) :: rs) = ONE t :: consTree(link (t1, t2), rs)

  (* cons (x : 'a, ts : 'a RList) = rs 'a RList
   * Where rs is a binary random access list and the first element of rs is now
   * x.
   *)
  fun cons (x, rs) = consTree(LEAF x, rs)

  (* lookupTree (i : int, t : 'a Tree) = x : 'a
   * Where x is the ith element in tree t where since t is a leaf complete
   * binary tree, x is the ith leaf from the left, starting at 0.
   *)
  fun lookupTree (0, LEAF x) = x
    | lookupTree (i, LEAF x) = raise Subscript
    | lookupTree (i, NODE (w, t1, t2)) = 
    if i < w div 2 then lookupTree (i, t1)
    else lookupTree (i - w div 2, t2)

  (* updateTree (i : int, x : 'a, t : 'a Tree = t' : 'a Tree
   * Where the elements of t' are the same as t, except the element in the ith
   * leaf has been replaced by x.
   *)
  fun updateTree (0, y, LEAF x) = LEAF y
    | updateTree (i, y, LEAF x) = raise Subscript
    | updateTree (i, y, NODE (w, t1, t2)) = 
    if i < w div 2 then NODE (w, updateTree(i, y, t1), t2)
    else NODE (w, t1, updateTree(i - w div 2, y, t2))

  (* lookup (i : int, rs : 'a RList) = x : 'a
   * Where x is the ith element in rs, a binary random access list.
   *)
  fun lookup (i, []) = raise Subscript
    | lookup (i, ONE t :: rs) = 
    if i < size t then lookupTree(i, t) else lookup (i - size t, rs)
    | lookup (i, TWO (t, t') :: rs) = 
    if i >= (size t + size t') then lookup (i - (size t + size t'), rs)
    else if i < size t then lookupTree (i, t)
    else lookupTree (i - size t, t')

  (* update (i : int, x : 'a, rs : 'a RList) = rs' : 'a RList
   * Where the element in the ith index is now x.
   *)
  fun update (i, y, []) = raise Subscript
    | update (i, y, ONE t :: rs) =
    if i < size t then ONE(updateTree(i, y, t)) :: rs
    else ONE t :: update(i - size t, y, rs)
    | update (i, y, TWO (t, t') :: rs) = 
    if i < size t then TWO(updateTree(i, y, t), t') :: rs
    else if i < (size t + size t') then 
            TWO(t, updateTree(i - size t, y, t')) :: rs
    else TWO (t, t') :: update (i - (size t + size t'), y, rs)

end
