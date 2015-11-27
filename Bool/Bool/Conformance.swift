extension Expr : BooleanLiteralConvertible {
  public init(booleanLiteral value: Bool) {
    self = .Const(value)
  }
}

extension Expr : StringLiteralConvertible {
  public init(extendedGraphemeClusterLiteral value: String) {
    self = .Var(value)
  }
  public init(stringLiteral value: String) {
    self = .Var(value)
  }
  public init(unicodeScalarLiteral value: String) {
    self = .Var(value)
  }
}


extension Expr {
  var precedence: Int {
    switch self {
    case .Const: return 9
    case Var: return 8
    case NOT: return 7
    case AND: return 5
    case OR: return 4
    }
  }
}

extension Expr : CustomStringConvertible {
  public var description: String {
    switch self {
    case let .Const(b): return b ? "1" : "0"
    case let .Var(s): return s
    case let .NOT(e): return "¬" + (e.precedence < 7 ? "(" + e.description + ")" : e.description)
    case let .AND(x): return x.map { e in e.precedence < 5 ? "(" + e.description + ")" : e.description }.joinWithSeparator("")
    case let .OR (x): return x.map { e in e.precedence < 4 ? "(" + e.description + ")" : e.description }.joinWithSeparator(" + ")
    }
  }
}

extension Expr: Hashable {
  public var hashValue: Int {
    return description.hashValue
  }
}


public func ==(lhs: Expr, rhs: Expr) -> Bool {
  switch (lhs,rhs) {
  case let (.Const(x),.Const(y)): return x == y
  case let (.AND(x),.AND(y)): return x == y
  case let (.Var(x),.Var(y)): return x == y
  case let (.OR(x),.OR(y)): return x == y
  case let (.NOT(x),.NOT(y)): return x == y
  default: return false
  }
}