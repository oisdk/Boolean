extension Expr {
  var precedence: Int {
    switch self {
    case .NOT  : return 6
    case .Const: return 7
    case .Var  : return 7
    case .OR   : return 3
    case .AND  : return 4
    }
  }
  var isBin: Bool {
    switch self {
    case .AND, .OR: return true
    default: return false
    }
  }
}

extension Expr : CustomStringConvertible {
  public var description: String {
    switch self {
    case let .Const(x): return x ? "1" : "0"
    case let .Var(s): return s
    case let .NOT(x): return "¬" + (x.precedence < 6 ? "(" + x.description + ")" : x.description)
    case let .AND(a,b):
      let lhs = a.precedence < 4 ? "(" + a.description + ")" : a.description
      let rhs = b.precedence < 4 ? "(" + b.description + ")" : b.description
      return lhs + " ∧ " + rhs
    case let .OR(a,b):
      let lhs = a.precedence < 3 ? "(" + a.description + ")" : a.description
      let rhs = b.precedence < 3 ? "(" + b.description + ")" : b.description
      return lhs + " ∨ " + rhs
    }
  }
}