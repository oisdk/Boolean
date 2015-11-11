public enum Expr : Equatable {
  case Var(String)
  case Const(Bool)
  indirect case AND(Set<Expr>)
  indirect case OR (Set<Expr>)
  indirect case NOT(Expr)
}

public func &&(lhs: Expr, rhs: Expr) -> Expr {
  switch (lhs,rhs) {
  case let (.Const(true),y): return y
  case let (x,.Const(true)): return x
  case let (.AND(x),.AND(y)): return .AND(x.union(y))
  case let (.AND(x),y): return .AND(x.union([y]))
  case let (x,.AND(y)): return .AND(y.union([x]))
  case let (x,y): return .AND([x,y])
  }
}

public func ||(lhs: Expr, rhs: Expr) -> Expr {
  switch (lhs,rhs) {
  case let (.Const(false),y): return y
  case let (x,.Const(false)): return x
  case let (.OR(x),.OR(y)): return .OR(x.union(y))
  case let (.OR(x),y): return .OR(x.union([y]))
  case let (x,.OR(y)): return .OR(y.union([x]))
  case let (x,y): return .OR([x,y])
  }
}

public prefix func !(e: Expr) -> Expr { return .NOT(e) }