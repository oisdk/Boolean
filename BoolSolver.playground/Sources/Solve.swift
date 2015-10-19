extension Expr {
  public func solve(n: String, _ x: Bool) -> Expr {
    switch self {
    case .Const: return self
    case let .Var(s)  : return s == n ? .Const(x) : self
    case let .NOT(a)  : return !a.solve(n,x)
    case let .AND(a,b): return a.solve(n,x) && b.solve(n,x)
    case let .OR (a,b): return a.solve(n,x) || b.solve(n,x)
    }
  }
  public func solve(vals: [String:Bool]) -> Expr {
    return vals.reduce(self) { (a,e) in a.solve(e.0, e.1) }
  }
}