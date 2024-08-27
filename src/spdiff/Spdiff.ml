type rewrite
  = Pexpr of { lhs : AST_generic.expr; rhs : AST_generic.expr }

(**
Basic dist/rewrites function skeleton:

dist(t, t) = 0
dist(c(ts), c(ts')) = dist_list(ts, ts')
dist(t, t') = size(t) + size(t')

dist_list([], ts') = ts' |> map size |> sum
dist_list(ts, []) = ts |> map size |> sum
dist_list(t:ts, t':ts') = min [
  - rm t; size(t) + dist_list(ts, t':ts')
  - add t'; size(t') + dist_list(t:ts, ts')
  - rewrite t->t'; dist(t, t') + dist_list(ts, ts')
]

dist(t,t') gives a measure of a tree-edit distance between t and t'. It should be
the case that dist is a metric on trees.

TODO: Since there multiple "instantiations" of this function where the tree-type is different, but the skeleton is the same
it would be nice if we didn't have to redo the function at each tree type. I.e. let's scrap the boilerplate! :-)


rewrites(t, t) = []
rewrites(c(ts), c'(ts')) =
  if c = c'
  then rewrites_list(ts, ts')
else (c(ts)->c'(ts')) : rewrites_list(ts, ts')

rewrites_list([], []) = []
rewrites_list(ts, ts') =
  - in principle the simple thing to do is to just do the cartesian
    product of terms but it's easy to see how that would be way too
    large and perhaps most of the time also "wrong"
    (we don't have safety defined so we have to produce good guesses)
  - given ts and ts' produce the smallest pairing; i.e. pairs of elements
    from ts and ts';

    given ts and ts' as follows

    1 f(42) 4
    1 f(117) 3

    =1 f(42)->f(117) 4->3
    0  2             2     = 4

    which is better than
    =1 -f(42) +f(117) -4+3
    0   2      2       1 1 = 6

    Example 2:

    f(42) 1 117
    g(52) f(10) 118

    Option1:
    +g(52) f(42)->f(10) -1 +118 -117
    2      2             1  1    1 = 7

    Option2:
    f(42)->g(52) 1->f(10) -117 +188
    3            3         1    1    = 8
*)


(* TODO[ja] It cannot be the case that this function does not have
   a way to be derived automatically *)
let rec expr_size (src: AST_generic.expr) =
  expr_kind_size src.e
and expr_kind_size (src: AST_generic.expr_kind) =
  match src with
  | L _ -> 1
  | Container (_containerOp, (_, exps, _)) ->
    List.fold_left
      (fun acc (exp: AST_generic.expr) -> acc + expr_kind_size exp.e)
      1
      exps
  | Comprehension (_, (_, (e, _for_or), _)) -> 1 + expr_size e (* TODO: for_or size*)
  | Record _
  | Constructor _
  | RegexpTemplate _
  | N _
  | IdSpecial _
  | Call _
  | New _
  | Xml _
  | Assign _
  | AssignOp _
  | LetPattern _
  | DotAccess _
  | ArrayAccess _
  | SliceAccess _
  | Lambda _
  | AnonClass _
  | Conditional _
  | Yield _
  | Await _
  | Cast _
  | Seq _
  | Ref _
  | DeRef _
  | Alias _
  | Ellipsis _
  | DeepEllipsis _
  | DisjExpr _
  | TypedMetavar _
  | DotAccessEllipsis _
  | StmtExpr _
  | OtherExpr _
  | RawExpr _ -> 0

let _expr_kind_dist (src: AST_generic.expr_kind) (tgt: AST_generic.expr_kind) =
  match src, tgt with
  | _ when AST_generic.equal_expr_kind src tgt  -> 0
  | L _, L _ -> 1 (* TODO[ja] look inside literal value *)
  (* We only shall find more precise diff for same ctor; *)
  | Container _, Container _
  | Comprehension _, Comprehension _
  | Record _, Record _
  | Constructor _, Constructor _
  | RegexpTemplate _, RegexpTemplate _
  | N _, N _
  | IdSpecial _, IdSpecial _
  | Call _, Call _
  | New _, New _
  | Xml _, Xml _
  | Assign _, Assign _
  | AssignOp _, AssignOp _
  | LetPattern _, LetPattern _
  | DotAccess _, DotAccess _
  | ArrayAccess _, ArrayAccess _
  | SliceAccess _, SliceAccess _
  | Lambda _, Lambda _
  | AnonClass _, AnonClass _
  | Conditional _, Conditional _
  | Yield _, Yield _
  | Await _, Await _
  | Cast _, Cast _
  | Seq _, Seq _
  | Ref _, Ref _
  | DeRef _, DeRef _
  | Alias _, Alias _
  | Ellipsis _, Ellipsis _ (* should probably not occur in programs *)
  | DeepEllipsis _, DeepEllipsis _
  | DisjExpr _, DisjExpr _
  | TypedMetavar _, TypedMetavar _
  | DotAccessEllipsis _, DotAccessEllipsis _
  | StmtExpr _, StmtExpr _ (* this makes for a nice mutual recursion *)
  | OtherExpr _, OtherExpr _
  | RawExpr _, RawExpr _ -> failwith "TODO"
  | _ -> expr_kind_size src + expr_kind_size tgt

let expr_rewrites (src: AST_generic.expr) (tgt: AST_generic.expr) =
  if AST_generic.equal_expr src tgt
  then []
  else
    (Pexpr { lhs = src; rhs = tgt }) ::
    match src.e, tgt.e with
    | _, _ -> []

let stmt_rewrites (src: AST_generic.stmt) (tgt: AST_generic.stmt) =
  match src.s, tgt.s with
  | _, _ when AST_generic.equal_stmt src tgt -> []
  | ExprStmt (srcExpr, _), ExprStmt (tgtExpr, _) -> expr_rewrites srcExpr tgtExpr
  | _ -> []

(** Find simple term-rewrites;
  TODO(s):
  - should they be "safe"?
  - should be minimum according to some edit-distance metric on AST
  - edit-distance relative to what transformation language
    - maybe just be inspired by Levenshtein edit distance on strings
*)
let rewrites (src: AST_generic.program) (tgt: AST_generic.program) =
  match src, tgt with
  | _, _ -> []
