let multiplex = Expr.OR(.AND(.Var("A"),.NOT(.Var("S"))),.AND(.Var("B"),.Var("S")))

multiplex.solve("S", val: .Const(false))