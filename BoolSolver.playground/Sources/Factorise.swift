extension Expr {
  var factorised: Expr {
    switch self {
    case let .OR(x):
      let (a,b) = x.partition { e -> Either<Set<Expr>,Expr> in
        if case let .AND(y) = e { return .Left(y) } else { return .Right(e) }
      }
      guard let c = a.lazy.flatten().mostFrequent else { return .OR(Set(b)) }
      let (d,e) = a.partition { (var s) -> Either<Set<Expr>,Set<Expr>> in
        if let _ = s.remove(c) {
          return .Left(s)
        } else {
          return .Right(s)
        }
      }
      return
        e.reduce(false) { (a,b) in a || b.reduce(true, combine: &&) } ||
        (c && d.reduce(false) { (a,b) in a || b.reduce(true, combine: &&) }.factorised)
    default: return self
    }
  }
}