extension Expr {
  var precedence: Int {
    switch self {
    case .NOT  : return 6
    case .Const: return 6
    case .Var  : return 6
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
    case let .OR(.AND(a,b),.AND(c,d))
      where a == !c && b == !d || a == !d && b == !c:
      let lhs = a.isBin ? "(" + a.description + ")" : a.description
      let e = !b
      let rhs = e.isBin ? "(" + e.description + ")" : e.description
      return lhs + " ⊻ " + rhs
    case let .AND(.OR(a,b),.OR(c,d))
      where a == !c && b == !d || a == !d && b == !c:
      let lhs = a.isBin ? "(" + a.description + ")" : a.description
      let e = !b
      let rhs = e.isBin ? "(" + e.description + ")" : e.description
      return lhs + " ⊻ " + rhs
    case let .NOT(x): return "¬" + (x.isBin ? "(" + x.description + ")" : x.description)
    case let .Const(x): return x ? "1" : "0"
    case let .AND(a,b):
      let lhs = a.precedence < 4 ? "(" + a.description + ")" : a.description
      let rhs = b.precedence < 4 ? "(" + b.description + ")" : b.description
      return lhs + " ∧ " + rhs
    case let .OR(a,b):
      let lhs = a.precedence < 3 ? "(" + a.description + ")" : a.description
      let rhs = b.precedence < 3 ? "(" + b.description + ")" : b.description
      return lhs + " ∨ " + rhs
    case let .Var(s): return s
    }
  }
}