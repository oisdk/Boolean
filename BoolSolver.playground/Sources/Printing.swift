extension Expr : CustomStringConvertible {
  public var description: String {
    switch self {
    case let .NOT(x): return "¬" + x.description
    case let .Const(x): return x ? "1" : "0"
    case let .AND(a,b): return "(" + a.description + " ∧ " + b.description + ")"
    case let .OR(a,b): return "(" + a.description + " ∨ " + b.description + ")"
    case let .Var(s): return s
    }
  }
}