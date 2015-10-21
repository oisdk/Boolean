public enum Expr : Equatable {
  case Const(Bool)
  case Var(String)
  indirect case NOT(Expr)
  indirect case AND(Expr,Expr)
  indirect case OR (Expr,Expr)
}

public func == (lhs: Expr, rhs: Expr) -> Bool {
  switch (lhs,rhs) {
  case let (.Const(l), .Const(r)  ): return l == r
  case let (.NOT (l),  .NOT  (r)  ): return l == r
  case let (.AND (a,b),.AND  (c,d)): return a==c && b==d || a==d && b==c
  case let (.OR  (a,b),.OR   (c,d)): return a==c && b==d || a==d && b==c
  case let (.Var(a),.Var(b)): return a == b
  default: return false
  }
}

public func < (lhs: Bool, rhs: Bool) -> Bool { return lhs && !rhs }
extension Bool : Comparable {}

