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

func inserted<K : Hashable,V>(k: K, _ v: V)(var _ d: [K:V]) -> [K:V] {
  d[k] = v
  return d
}

public func powerSet<K : Hashable>(var s: Set<K>) -> [[K:Bool]] {
  guard let k = s.popFirst() else { return [[:]] }
  let rest = powerSet(s)
  return rest.map(inserted(k, true)) + rest.map(inserted(k, false))
}

extension Expr {
  var minTerms: [[String:Bool]] {
    return powerSet(allVars)
  }
}

