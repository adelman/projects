(* Matt Adelman
 * Stream Implementation
 *)

 (* STREAM signature from the book. *)
signature STREAM = 
sig
  datatype 'a Stream = Nil | Cons of 'a * (unit -> 'a Stream)

  val append  : 'a Stream * 'a Stream -> 'a Stream
  val take    : int * 'a Stream -> 'a Stream
  val drop    : int * 'a Stream -> 'a Stream
  val reverse : 'a Stream -> 'a Stream
end

(* Modification of Stream structure from the book. *)
structure Stream : STREAM = 
struct

  (* Since SML does not support the $-notation of Okasaki, we use streams
   * that are delayed by the unit value. Therefore, the tail of an 'a Stream
   * will only be evalueated if applied to the unit.
   *)
  datatype 'a Stream = Nil | Cons of 'a * (unit -> 'a Stream)

  (* append xs : 'a Stream, ys : 'a Stream = zs : 'a Stream
   * Where if xs = (x0, x1,...) and ys = (y0, y1,...) then
   * zs = (x0, x1,..., y0, y1,...).
   *)
  fun append (Nil, xs) = xs
    | append ((Cons(x, xs)), ys) = Cons(x, fn () => append(xs(), ys))

  (* take n : int, xs : 'a Stream = zs : 'a Stream
   * Where if xs = (x0, x1,...,xn, x(n+1),...) then
   * zs = (x0, x1,...,xn)
   *)
  fun take (0, xs) = Nil
    | take (n, Nil) = Nil
    | take (n, (Cons(x, xs))) = Cons(x, fn () => take (n-1, xs()))

  (* drop n : int, xs : 'a Stream = zs : 'a Stream
   * Where if xs = (x0, x1,...,xn, x(n+1),...) then
   * zs = (x(n+1), x(n+2),...)
   *)
  fun drop (n, xs) = 
  let
    fun drop' (0, xs) = xs
      | drop' (n, Nil) = Nil
      | drop' (n, (Cons(x, xs))) = drop' (n - 1, (xs()))
  in
    drop' (n, xs)
  end

  (* reverse (x0,x1,...,xn) : 'a Stream = (xn,x(n-1),...,x0) : 'a Stream
   * We have the precondition that the Stream must be finite.
   *)
  fun reverse xs = 
  let
    fun reverse' (Nil, r) = r
      | reverse' ((Cons(x, xs)), r) = reverse' (xs(), Cons(x, fn () => r))
  in
    reverse' (xs, Nil)
  end

  (* insert_sort xs : 'a Stream, cmp : ('a * 'a -> int) = ys : 'a Stream
   * Where keys(ys) = keys(xs) sorted in non-decreasing order. 
   * cmp is a functions such that cmp(a, b) < 0 if a is less than b, cmp(a,b) =
   * 0 is a = b, and cmp(a,b) > 0 otherwise.
   * We have the precondition that xs is finite.
   *)
  fun insert_sort cmp xs = 
  let
    (* insert x : 'a, ys : 'a Stream = zs : 'a Stream 
     * Where keys(zs) = keys(ys) U {x}, and zs is sorted in non-decreasing
     * order according to cmp. We have the precondition that ys is sorted in 
     * non-decreasing order according to cmp. *)
    fun insert x Nil = Cons(x, fn () => Nil)
      | insert x (Cons(y, ys)) =
      if cmp(x, y) <= 0 
      then Cons(x, fn () => (Cons(y, ys)))
      else Cons(y, fn () => insert x (ys()))

    (* insert_sort' xs : 'a Stream = ys : 'a Stream
     * Where ys is the sorted version of xs according to cmp. 
     *)
    fun insert_sort' Nil = Nil
      | insert_sort' (Cons(x, xs)) = 
      insert x (insert_sort' (xs()))
  in
    insert_sort' xs
  end

end
