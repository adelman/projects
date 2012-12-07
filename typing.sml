(* Type inference algorithm for a basic functional programming language. 
 * Matt Adelman
 *
 * The abstract syntax tree code was provided by my professor, so it is not
 * included here.
 *)
structure Typing =
struct

  structure Ast = Ast

  exception infer_error

  (* Type variables are non-negative integers.  Think of the type
  *  variable i as really standing for alpha_i.
  *)
  type var = int

  (* The type of types. *)
  datatype t = V of var | Int | Bool | Unit |
        Arrow of t*t | List of t | Ref of t

  fun toString (V(i)) =
    if i < 26 then Char.toString(Char.chr(97+i))
    else if i < 52 then Char.toString(Char.chr(64+(i-26)))
         else "(a{" ^ Int.toString(i-52) ^ "})"
    | toString (Int) = "int"
    | toString (Bool) = "bool"
    | toString (Arrow(rho, tau)) = "(" ^ toString(rho) ^ ") -> (" ^ 
                                        toString(tau) ^ ")"
    | toString (List(rho)) = "[" ^ toString(rho) ^ "]"
    | toString (Ref(rho)) = toString(rho) ^ " ref"
    | toString (Unit) = "()"

  fun eqns_toString [] = ""
    | eqns_toString ((sigma, tau) :: eqns) =
        (toString sigma) ^ " = " ^ (toString tau) ^ ", " ^ (eqns_toString eqns)

  (* A labeled AST is just like an AST, except each node is labeled
  *  by a type.  In fact, the label is always a type variable.  NilList
  *  is special; it is labeled by a pair of the form (alpha, List(beta)).
  *  We need to do this because on the one hand we must have a type-variable
  *  label alpha, but on the other [] must be assigned the type List(beta)
  *  where beta and alpha are fresh and distinct.  The reason we need
  *  alpha (i.e., not just List(beta)) is so that the expression [] 
  *  generates the equation alpha = List(beta).
  *)
  datatype lbl_ast = Ident of Ast.ident*t |
    Number of int*t | Boolean of bool*t |
    UnOp of Ast.unop*lbl_ast*t | BinOp of Ast.binop*lbl_ast*lbl_ast*t |
    NilList of t | UNIT of t |
    Cond of lbl_ast*lbl_ast*lbl_ast*t |
    Abs of (Ast.ident*t)*lbl_ast*t |
    App of lbl_ast*lbl_ast*t |
    Letrec of (Ast.ident*t)*lbl_ast*lbl_ast*t


  (* An equation is just a pair of types. *)
  type eqn = t*t

  (* A type environment maps type variables to types. *)
  type 'a env = 'a -> t

  (* Raised when an environment is applied to an identifier not in its domain.
  *)
  exception env_error ;

  (* The empty environment. *)
  val env_empty = (fn v => raise env_error) : 'a env

  (* env_update (env, id, v) = env', where env' = [v/id]env. *)
  fun env_update(e, id, v) = fn x => if x = id then v else e(x) ;

  (* env_get (env, id) = env(id) (i.e., the value id is mapped to by env. *)
  fun env_get(e, id) = e(id) ;

  (* env_del (env, id) is the environment obtained by deleting the binding
  *  for id from env.
  *)
  fun env_del(e, id) = fn x => if x = id then raise env_error else e x

  exception subst_domain
  type type_subst = var -> t
  val id_subst : type_subst = fn i => (V i)

  fun apply (s : type_subst) (V(i) : t) : t = 
        (s(i) handle subst_domain => (V i))
    | apply s (Int) = Int
    | apply s (Bool) = Bool
    | apply s (Unit) = Unit
    | apply s (Arrow (sigma, tau)) = Arrow (apply s sigma, apply s tau)
    | apply s (List sigma) = List(apply s sigma)
    | apply s (Ref sigma) = Ref(apply s sigma)

  fun compose (s2 : type_subst) (s1 : type_subst) : type_subst =
    fn i => apply s2 (apply s1 (V i))

  fun type_substToString (s : type_subst) (n : int) : string =
    if n = ~1 then ""
    else (type_substToString s (n-1)) ^ (Int.toString n) ^ " |-> " ^
          (toString (apply s (V n))) ^ "; "

  fun max2(a, b) =
    if a > b then a else b

  fun max3(a, b, c) =
    if a > b then max2(a, c)
    else max2(b, c)

  (* label: Ast.expr -> LabAst
  *  label e => the same AST as e, but with each node labeled by a
  *  type variable as described in Mitchell.
  *)
  fun label e =
  let
    (* label_env e eta min_lbl = (e', n) where e' is the labeled AST
    *  corresponding to e with identifiers labeled according to eta
    *  and n is the maximum value type variable used in the labeling
    *  of e'.  All fresh type variables V(j) used in labeling e' satisfy
    *  j >= min_lbl.
    *)
    fun label_one_child e eta min_lbl =
    let
      val (e', n) = label_env e eta min_lbl
    in
      (e', max2(min_lbl, n))
    end

    and label_two_children e0 e1 eta min_lbl =
          let
            val (e1', n1) = label_env e1 eta min_lbl
            val (e0', n0) = label_env e0 eta (max2(min_lbl, n1+1))
          in
            (e0', e1', max2(n0, n1))
          end

    and label_env (Ast.Ident(x)) eta min_lbl = 
          (case eta x of
               V(i) => (Ident(x, eta x), i)
             | _ => raise infer_error)

      (* Literals. *)
      | label_env (Ast.Number(n)) eta min_lbl = (Number(n, Int), min_lbl)
      | label_env (Ast.Boolean b) eta min_lbl = (Boolean (b, Bool), min_lbl)
      | label_env (Ast.NilList) eta min_lbl = (NilList(List(V min_lbl)), 
                                                min_lbl)

      (* Unary operators. *)
      | label_env (Ast.UnOp(Ast.NEG, e)) eta min_lbl =
          let
            val (e', n) = label_one_child e eta min_lbl
          in
            (UnOp(Ast.NEG, e', Int), max2(min_lbl, n))
          end
      | label_env (Ast.UnOp(Ast.NOT, e)) eta min_lbl =
          let
            val (e', n) = label_one_child e eta min_lbl
          in
            (UnOp(Ast.NOT, e', Bool), max2(min_lbl, n))
          end
      | label_env (Ast.UnOp(rator as (Ast.HEAD | Ast.TAIL), e)) eta min_lbl =
          let
            val (e', n) = label_one_child e eta (min_lbl+1)
          in
            (UnOp(rator, e', V(min_lbl)), max2(min_lbl, n))
          end
      | label_env (Ast.UnOp(Ast.REF, e)) eta min_lbl = 
          let
            val (e', n) = label_one_child e eta (min_lbl + 1)
          in
            (UnOp(Ast.REF, e', Ref(V(min_lbl))), max2(min_lbl, n))
          end
      | label_env (Ast.UnOp(Ast.DEREF, e)) eta min_lbl = 
          let
            val (e', n) = label_one_child e eta (min_lbl + 1)
          in
            (UnOp(Ast.DEREF, e', V(min_lbl)), max2(min_lbl, n))
          end

      (* Binary operators. *)
      | label_env 
            (Ast.BinOp(rator as (Ast.PLUS | Ast.SUB | Ast.TIMES | Ast.DIV),
                       e0, e1)) eta min_lbl =
          let
            val (e0', e1', n) = label_two_children e0 e1 eta min_lbl
          in
            (BinOp(rator, e0', e1', Int), max2(min_lbl, n))
          end
      | label_env 
            (Ast.BinOp(rator as (Ast.LT | Ast.LE | Ast.GT | Ast.GE | Ast.EQ |
                                 Ast.NE | Ast.AND | Ast.OR),
                       e0, e1)) eta min_lbl =
          let
            val (e0', e1', n) = label_two_children e0 e1 eta min_lbl
          in
            (BinOp(rator, e0', e1', Bool), max2(min_lbl, n))
          end
      | label_env 
            (Ast.BinOp(Ast.CONS, e0, e1)) eta min_lbl =
          let
            val (e0', e1', n) = label_two_children e0 e1 eta (min_lbl+1)
          in
            (BinOp(Ast.CONS, e0', e1', List(V(min_lbl))), max2(min_lbl, n))
          end
     | label_env
            (Ast.BinOp(Ast.ASSIGN, e0, e1)) eta min_lbl = 
          let
            val (e0', e1', n) = label_two_children e0 e1 eta (min_lbl+1)
          in
            (BinOp(Ast.ASSIGN, e0', e1', V(min_lbl)), max2(min_lbl, n))
          end
     | label_env
            (Ast.BinOp(Ast.SEQ, e0, e1)) eta min_lbl = 
          let
            val (e0', e1', n) = label_two_children e0 e1 eta (min_lbl+1)
          in
            (BinOp(Ast.SEQ, e0', e1', V(min_lbl)), max2(min_lbl, n))
          end

      (* Conditional. *)
      | label_env (Ast.Cond(e, e0, e1)) eta min_lbl =
          let
            val (e', n') = label_one_child e eta (min_lbl+1)
            val (e0', e1', n'') = label_two_children e0 e1 eta (n'+1)
          in
            (Cond(e', e0', e1', V(min_lbl)), max3(min_lbl, n', n''))
          end

      (* Abstraction and application. *)
      | label_env (Ast.App(e0, e1)) eta min_lbl = 
          let
            val (e0', e1', n) = label_two_children e0 e1 eta (min_lbl+1)
          in
            (App(e0', e1', V(min_lbl)), max2(min_lbl, n))
          end
      | label_env (Ast.Abs(x, e)) eta min_lbl =
          let
            val (e', n) = label_env e (env_update(eta, x, V(min_lbl+1))) 
            (min_lbl+2)
          in
            (Abs((x, V(min_lbl+1)), e', V(min_lbl)), max2(min_lbl, n))
          end

      (* Letrec. *)
      | label_env (Ast.Letrec(f, defn, body)) eta min_lbl =
          let
            val (defn', body', n) = label_two_children defn body
                                      (env_update(eta, f, V(min_lbl+1)))
                                      (min_lbl+2)
          in
            (Letrec((f, V(min_lbl+1)), defn', body', V(min_lbl)), 
                                                        max2(min_lbl, n))
          end
  in
    #1(label_env e env_empty 0)
  end

  fun get_lbl (Ident(_, lbl)) = lbl
    | get_lbl (Abs(_, _, lbl)) = lbl
    | get_lbl (App(_,_,lbl)) = lbl
    | get_lbl (Number(_, lbl)) = lbl
    | get_lbl (Boolean(_, lbl)) = lbl
    | get_lbl (NilList(lbl)) = lbl
    | get_lbl (UNIT(lbl)) = lbl
    | get_lbl (UnOp(_, _, lbl)) = lbl
    | get_lbl (BinOp(_, _, _, lbl)) = lbl
    | get_lbl (Cond(_, _, _, lbl)) = lbl
    | get_lbl (Letrec(_, _, _, lbl)) = lbl

  (* equations : lbl_ast -> eqn list
  *  equations e => a list of equations between types produced by
  *  analyzing the labeled AST e as described in Mitchell.
  *)
  fun equations (Ident(_, _)) = []
    | equations (Number(n, Int)) = []
    | equations (Boolean(b, Bool)) = []
    | equations (UnOp(Ast.NEG, e, sigma)) = 
        (get_lbl e, Int) :: (equations e)
    | equations (UnOp(Ast.NOT, e, sigma)) = 
        (get_lbl e, Bool) :: (equations e)
    | equations (UnOp(Ast.HEAD, e, sigma)) =
        (get_lbl e, List(sigma)) :: (equations e)
    | equations (UnOp(Ast.TAIL, e, sigma)) =
        (get_lbl e, sigma) :: (equations e)
    | equations (UnOp(Ast.REF, e, sigma)) = 
        (Ref(get_lbl e), sigma) :: (equations e)
    | equations (UnOp(Ast.DEREF, e, sigma)) = 
        (get_lbl e, Ref(sigma)) :: (equations e)
    | equations (BinOp((Ast.PLUS | Ast.SUB | Ast.TIMES | Ast.DIV), 
                       e0, e1, sigma)) =
        (get_lbl e0, Int) :: (get_lbl e1, Int) ::
            ((equations e0) @ (equations e1))
    | equations (BinOp((Ast.AND | Ast.OR), 
                       e0, e1, sigma)) =
        (get_lbl e0, Bool) :: (get_lbl e1, Bool) ::
            ((equations e0) @ (equations e1))
    | equations (BinOp((Ast.LE | Ast.LT | Ast.GE | Ast.GT), 
                       e0, e1, sigma)) =
        (get_lbl e0, Int) :: (get_lbl e1, Int) ::
            ((equations e0) @ (equations e1))
    | equations (BinOp((Ast.EQ | Ast.NE), e0, e1, sigma)) =
        (get_lbl e0, get_lbl e1) :: ((equations e0) @ (equations e1))
    | equations (BinOp(Ast.CONS, e0, e1, sigma)) =
        let
          val e0_lbl = get_lbl e0
          val e1_lbl = get_lbl e1
        in
          (sigma, e1_lbl) :: (e1_lbl, List(e0_lbl)) :: 
            ((equations e0) @ (equations e1))
        end
    | equations (BinOp(Ast.ASSIGN, e0, e1, sigma)) = 
        let
          val e0_lbl = get_lbl e0
          val e1_lbl = get_lbl e1
        in
          (sigma, Unit) :: (e0_lbl, Ref(e1_lbl)) ::
          ((equations e0) @ (equations e1))
        end
    | equations (BinOp(Ast.SEQ, e0, e1, sigma)) = 
        let
          val e0_lbl = get_lbl e0
          val e1_lbl = get_lbl e1
        in
          (sigma, e1_lbl) :: ((equations e0) @ (equations e1))
        end
    | equations (NilList(List(V n))) = []
    | equations (UNIT(V n)) = []
    | equations (Cond(e_if, e_then, e_else, sigma)) = 
        let
          val ethen_lbl = get_lbl e_then
          val eelse_lbl = get_lbl e_else
        in
          (sigma, ethen_lbl) :: (get_lbl e_if, Bool) :: 
            (sigma, eelse_lbl) :: 
            ((equations e_if) @ (equations e_then) @ (equations e_else))
        end
    | equations (App(e0, e1, sigma)) =
        (get_lbl e0, Arrow(get_lbl e1, sigma)) :: 
            ((equations e0) @ (equations e1))
    | equations (Abs((x, rho), e, sigma)) =
        (sigma, Arrow(rho, get_lbl e)) :: (equations e)
    | equations (Letrec((f, V m), defn, body, V n)) =
        (V m, get_lbl defn) :: (V n, get_lbl body) :: 
            ((equations defn) @ (equations body))

  (* subst_type sigma i tau = [sigma/V(i)]tau *)
  fun subst_type sigma i (V(j)) = if i = j then sigma else V(j)
    | subst_type sigma i (Int) = Int
    | subst_type sigma i (Bool) = Bool
    | subst_type sigma i (Unit) = Unit
    | subst_type sigma i (Arrow(rho, tau)) = Arrow(subst_type sigma i rho,
                                                   subst_type sigma i tau)
    | subst_type sigma i (List(tau)) = List(subst_type sigma i tau)
    | subst_type sigma i (Ref(tau)) = Ref(subst_type sigma i tau)

  (* subst_eqn sigma i (rho, tau) = ([sigma/V(i)]rho, [sigma/V(i)]tau). *)
  fun subst_eqn sigma i (rho, tau) = (subst_type sigma i rho,
                                      subst_type sigma i tau)

  (* subst_eqns sigma i [(r0,t0),...,(rK,tK)] =
  *     [([sigma/V(i)]r0, [sigma/V(i)]t0),...,([sigma/V(i)]rK, [sigma/V(i)]tK)].
  *)
  fun subst_eqns sigma i [] = []
    | subst_eqns sigma i (eqn :: eqns) = 
        (subst_eqn sigma i eqn) :: (subst_eqns sigma i eqns)

  (* occurs : int -> ttype -> bool.
  *  occurs i sigma = true if V(i) occurs in sigma, false o/w.
  *)
  fun occurs i (V(j)) = (i = j)
    | occurs i (Int) = false
    | occurs i (Bool) = false
    | occurs i (Unit) = false
    | occurs i (Arrow(sigma, tau)) = (occurs i sigma) orelse (occurs i tau)
    | occurs i (List(sigma)) = occurs i sigma
    | occurs i (Ref(sigma)) = occurs i sigma

  (* unify : eqn list -> type_subst
  *  unify eqns = s where s is an mgu for eqns; if eqns has no unifier,
  *     then unify eqns raises infer_error.
  *)
  fun unify ([] : eqn list) : type_subst = id_subst
    | unify (((Int, Int) :: eqns) | ((Bool, Bool) :: eqns)) = unify eqns
    | unify ((Unit, Unit) :: eqns) = unify eqns
    | unify ((V i, V j) :: eqns) = 
        if i = j then unify eqns
        else compose (unify (subst_eqns (V j) i eqns))
                (fn k => if k=i then (V j) else raise subst_domain)
    | unify ((Arrow (sigma, tau), Arrow (sigma', tau')) :: eqns) =
        unify ((sigma, sigma') :: (tau, tau') :: eqns)
    | unify ((List sigma, List sigma') :: eqns) = 
        unify ((sigma, sigma') :: eqns)
    | unify ((Ref sigma, Ref sigma') :: eqns) = 
        unify ((sigma, sigma') :: eqns)
    | unify (((V i, sigma) :: eqns) | ((sigma, V i) :: eqns)) =
        if occurs i sigma then raise infer_error 
        else compose (unify (subst_eqns sigma i eqns))
                (fn j => if j=i then sigma else raise subst_domain)
    | unify _ = raise infer_error

  (* infer e => the most general type of e, inferred using Hindley-Milner.
  *  Specified by TYPING.
  *)
  fun infer(e) =
  let
    val e_lbl = label e
    val lbl = get_lbl e_lbl
    val eqns = equations e_lbl : eqn list

    val mgu = SOME(unify eqns) handle infer_error => NONE : type_subst option

    (*
    val do_print = print ("\nlbl : " ^ toString(lbl) ^ "\n" ^
           "eqns : " ^ eqns_toString(eqns) ^ "\n"
           "mgu : " ^ (case mgu of SOME(s)=>(type_substToString s 5) | NONE =>
           "<none>") ^ "\n")
    *)
  in
    case mgu of
         SOME(s) => SOME(apply s lbl)
       | NONE => NONE
  end

end
