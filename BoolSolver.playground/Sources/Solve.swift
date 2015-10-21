extension Expr {
  public func solve(vals: [String:Bool]) -> Bool {
    switch self {
    case let .Const(x): return x
    case let .Var(s)  : return vals[s]!
    case let .NOT(a)  : return !a.solve(vals)
    case let .AND(a,b): return a.solve(vals) && b.solve(vals)
    case let .OR (a,b): return a.solve(vals) || b.solve(vals)
    }
  }
}