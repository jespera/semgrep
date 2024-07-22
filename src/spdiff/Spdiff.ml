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

Since there multiple "instantiations" of this function where the tree-type is different, but the skeleton is the same
it would be nice if we didn't have to redo the function at each tree type. I.e. let's scrap the boilerplate! :-)
*)


let expr_kind_dist (src: AST_generic.expr_kind) (tgt: AST_generic.expr_kind) =
  match src, tgt with
  | _ when AST_generic.equal_expr_kind src tgt  -> 0
  | _ -> 42 

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