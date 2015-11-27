extension Expr {
  public var variables: Set<String> {
    switch self {
    case let .Var(s): return [s]
    case .Const     : return []
    case let .AND(x): return x.lazy.map { e in e.variables }.reduce([]) { (a,b) in a.union(b) }
    case let .OR (x): return x.lazy.map { e in e.variables }.reduce([]) { (a,b) in a.union(b) }
    case let .NOT(e): return e.variables
    }
  }
}

extension Expr {
  public func subIn(s: Set<String>) -> Bool {
    switch self {
    case let .Var(x)  : return s.contains(x)
    case let .Const(x): return x
    case let .AND(x)  : return x.lazy.map { y in y.subIn(s) }.reduce(true ) { (a,b) in a && b }
    case let .OR (x)  : return x.lazy.map { y in y.subIn(s) }.reduce(false) { (a,b) in a || b }
    case let .NOT(x)  : return !x.subIn(s)
    }
  }
}

private func converge<T : Equatable>(f: T -> T, var x: T) -> T {
  while true {
    let y = f(x)
    if y != x { x = y } else { return y }
  }
}

extension Expr {
  public var simplified: Expr {
    let primeImplicants = converge(minOnce, x: minTerms)
    let v = Array(variables)
    return primeImplicants.reduce(false) { (a,e) in
      a || v.indices.reduce(Expr.Const(true)) { (aa,i) in
        switch e[i] {
        case .T: return aa && Expr.Var(v[i])
        case .F: return aa && Expr.NOT(.Var(v[i]))
        case .M: return aa
        }
      }
    }.factorised
  }
}
