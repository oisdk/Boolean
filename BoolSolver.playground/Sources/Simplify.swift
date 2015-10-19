public enum Expr : Equatable {
  case Const(Bool)
  case Var(String)
  indirect case NOT(Expr)
  indirect case AND(Expr, Expr)
  indirect case OR (Expr, Expr)
}

public func == (lhs: Expr, rhs: Expr) -> Bool {
  switch (lhs,rhs) {
  case let (.Const(l),.Const(r)  ): return l == r
  case let (.NOT (l),  .NOT (r)  ): return l == r
  case let (.AND (a,b),.AND (c,d)): return a==c && b==d || a==d && b==c
  case let (.OR  (a,b),.OR  (c,d)): return a==c && b==d || a==d && b==c
  case let (.Var(a),.Var(b)): return a == b
  default: return false
  }
}

extension Expr {
  public var simplified: Expr {
    switch self {
    case .Const, .Var: return self
    case let .NOT(x):
      switch x.simplified {
      case let .AND(.NOT(a),.NOT(b)): return a || b
      case let .OR (.NOT(a),.NOT(b)): return a && b
      case let .NOT(y)  : return y
      case let .Const(y): return .Const(!y)
      case let y        : return .NOT(y)
      }
    case let .AND(a,b):
      switch (a.simplified,b.simplified) {
      case (_,.Const(false)),(.Const(false),_): return .Const(false)
      case let (x,.Const(true)): return x
      case let (.Const(true),x): return x
      case let (.OR(w,x),.OR(y,z)):
        if w == y { return w || (x && z) }
        if w == z { return w || (x && y) }
        if x == z { return x || (w && y) }
        return .AND(.OR(w,x),.OR(y,z))
      case let (x,y): return x == y ? x : .AND(x,y)
      }
    case let .OR(a,b):
      switch (a.simplified,b.simplified) {
      case (_,.Const(true)),(.Const(true),_): return .Const(true)
      case let (x,.Const(false)): return x
      case let (.Const(false),x): return x
      case let (.AND(w,x),.AND(y,z)):
        if w == y { return w && (x || z) }
        if w == z { return w && (x || y) }
        if x == z { return x && (w || y) }
        return .OR(.AND(w,x),.AND(y,z))
      case let (x,y): return x == y ? x : .OR(x, y)
      }
    }
  }
}