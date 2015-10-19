extension Expr {
  var allVars: Set<String> {
    switch self {
    case .Const: return []
    case let .Var(s  ): return [s]
    case let .AND(a,b): return a.allVars.union(b.allVars)
    case let .OR (a,b): return a.allVars.union(b.allVars)
    case let .NOT(a  ): return a.allVars
    }
  }
}

func inserted<K : Hashable,V>(k: K, _ v: V)(var d: [K:V]) -> [K:V] {
  d[k] = v
  return d
}

func powerSet(var s: Set<String>) -> [[String:Expr]] {
  guard let x = s.popFirst() else { return [[:]] }
  let rest = powerSet(s)
  return rest.map(inserted(x, true)) + rest.map(inserted(x, false))
}

extension Expr {
  public var truthTable: String {
    let names = allVars
    return names.sort().joinWithSeparator(" | ") + " | \n" +
    powerSet(names).map { vars in
      let lhs = vars
        .sort {(a,b) in a.0 < b.0}
        .map  {(_,b) in b.description }
        .joinWithSeparator(" | ")
      let rhs = solve(vars).description
      return lhs + " | " + rhs
    }.sort().joinWithSeparator("\n")
  }
}