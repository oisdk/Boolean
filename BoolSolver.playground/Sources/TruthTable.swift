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
  public var asBool: Bool? {
    guard case let .Const(x) = self else { return nil }
    return x
  }
}

func powerSet(var s: Set<String>) -> [[String:Bool]] {
  guard let x = s.popFirst() else { return [[:]] }
  let rest = powerSet(s)
  return rest.map { (var d) in
    d[x] = true
    return d
  } + rest.map { (var d) in
    d[x] = false
    return d
  }
}

extension Expr {
  public var truthTable: String {
    let names = allVars
    return names.sort().joinWithSeparator(" | ") + " | \n" +
    powerSet(names).map { vars in
      let lhs = vars
        .sort {(a,b) in a.0 < b.0}
        .map { (_,b) in b ? "1" : "0" }
        .joinWithSeparator(" | ")
      let rhs = solve(vars).simplified.asBool! ? "1" : "0"
      return lhs + " | " + rhs
    }.sort().joinWithSeparator("\n")
  }
}