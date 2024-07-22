type rewrite = Pexpr of { lhs : AST_generic.expr; rhs : AST_generic.expr }

val stmt_rewrites : AST_generic.stmt -> AST_generic.stmt -> rewrite list
val rewrites : AST_generic.program -> AST_generic.program -> rewrite list
