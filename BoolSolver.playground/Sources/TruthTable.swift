extension Expr {
  internal var allVars: Set<String> {
    switch self {
    case .Const: return []
    case let .Var(s  ): return [s]
    case let .AND(a,b): return a.allVars.union(b.allVars)
    case let .OR (a,b): return a.allVars.union(b.allVars)
    case let .NOT(a  ): return a.allVars
    }
  }
}
