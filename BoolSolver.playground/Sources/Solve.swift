extension Expr {
  public func solve(v: String, val: Bool) -> Expr {
    switch self {
    case let .Var(s): return s == v ? .Const(val) : self
    case .Const: return self
    case let .NOT(x): return Expr.NOT(x.solve(v, val: val)).simplified
    case let .AND(a,b): return Expr.AND(a.solve(v, val: val),b.solve(v, val: val)).simplified
    case let .OR(a,b): return Expr.OR(a.solve(v, val: val),b.solve(v, val: val)).simplified
    }
  }
  public func solve(vals: [String:Bool]) -> Expr {
    return vals.reduce(self) { (a,e) in a.solve(e.0, val: e.1) }
  }
}