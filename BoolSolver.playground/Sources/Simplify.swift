public enum Expr : Equatable {
  case Const(Bool)
  case Var(String)
  indirect case NOT (Expr)
  indirect case AND (Expr, Expr)
  indirect case OR  (Expr, Expr)
}

public func == (lhs: Expr, rhs: Expr) -> Bool {
  switch (lhs,rhs) {
  case let (.Const(l),.Const(r)  ): return l == r
  case let (.NOT (l),  .NOT (r)  ): return l == r
  case let (.AND (a,b),.AND (c,d)): return a == b && c == d
  case let (.OR  (a,b),.OR  (c,d)): return a == b && c == d
  case let (.Var(a),.Var(b)): return a == b
  default: return false
  }
}

extension Expr {
  public init(_ a: Expr, AND  b: Expr) { self = .AND(a, b) }
  public init(_ a: Expr, OR   b: Expr) { self = .OR(a, b) }
  public init(_ a: Expr, NAND b: Expr) { self = .NOT(.AND(a, b)) }
  public init(_ a: Expr, NOR  b: Expr) { self = .NOT(.OR(a, b))  }
  public init(_ a: Expr, XOR  b: Expr) { self = .OR(.AND(a,.NOT(b)),.AND(.NOT(a), b))}
  public init(_ a: Expr, XNOR b: Expr) { self = .NOT(Expr(a, XOR: b)) }
  public init(NOT: Expr) { self = .NOT(NOT) }
  public init(_ b: Bool) { self = .Const(b) }
  public init(_ s: String) { self = .Var(s) }
}



extension Expr {
  public var simplified: Expr {
    switch self {
    case .Const, .Var: return self
    case let .NOT(x):
      switch x.simplified {
      case let .NOT(y): return y
      case let .Const(y): return .Const(!y)
      case let y: return .NOT(y)
      }
    case let .AND(a,b):
      switch (a.simplified,b.simplified) {
      case (_,.Const(false)),(.Const(false),_): return .Const(false)
      case let (x,.Const(true)): return x
      case let (.Const(true),x): return x
      case let (.OR(w,x),.OR(y,z)):
        if w == y { return Expr.OR(w,Expr.AND(x,z).simplified).simplified }
        if w == z { return Expr.OR(w,Expr.AND(x,y).simplified).simplified }
        if x == z { return Expr.OR(x,Expr.AND(w,y).simplified).simplified }
        return .AND(.OR(w,x),.OR(y,z))
      case let (x,y): return x == y ? x : .AND(x,y)
      }
    case let .OR(a,b):
      switch (a.simplified,b.simplified) {
      case (_,.Const(true)),(.Const(true),_): return .Const(true)
      case let (x,.Const(false)): return x
      case let (.Const(false),x): return x
      case let (.AND(w,x),.AND(y,z)):
        if w == y { return Expr.AND(w,Expr.OR(x,z).simplified).simplified }
        if w == z { return Expr.AND(w,Expr.OR(x,y).simplified).simplified }
        if x == z { return Expr.AND(x,Expr.OR(w,y).simplified).simplified }
        return .OR(.AND(w,x),.AND(y,z))
      case let (x,y): return x == y ? x : .OR(x,y)
      }
    }
  }
}