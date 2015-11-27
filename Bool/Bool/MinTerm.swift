internal enum BoolTerm: Int { case T, F, M }

internal func tryMerge(var a: MinTerm, _ b: MinTerm) -> MinTerm? {
  var foundOne = false
  for i in b.x.indices where a[i] != b[i] {
    if foundOne { return nil }
    a[i] = .M
    foundOne = true
  }
  return foundOne ? a : nil
}

internal struct MinTerm : Equatable { private var x: [BoolTerm] }

func ==(lhs: MinTerm, rhs: MinTerm) -> Bool {
  for i in lhs.x.indices { if lhs[i] != rhs[i] { return false } }
  return true
}

extension MinTerm : Hashable {
  var hashValue: Int {
    var c = 0
    var m = 1
    for e in x {
      c += m * e.rawValue
      m *= 3
    }
    return c
  }
}

extension MinTerm {
  init(c: Int, m: Int) {
    self.x = (0..<c).map { i in m & (1<<i) != 0 ? .T : .F }
  }
  func toVarSet<T: Hashable>(a: [T]) -> Set<T> {
    var res: Set<T> = []
    for i in a.indices {
      if case .T = x[i] { res.insert(a[i]) }
    }
    return res
  }
}

extension MinTerm {
  internal subscript(i: Int) -> BoolTerm {
    get { return x[i] }
    set { x[i] = newValue }
  }
}

extension Expr {
  internal var minTerms: Set<MinTerm> {
    let v = Array(variables)
    return Set((0..<(1<<v.count)).lazy
      .map{ i in MinTerm.init(c: v.count, m: i) }
      .filter { mt in self.subIn(mt.toVarSet(v)) }
    )
  }
}

internal func minOnce(mt: Set<MinTerm>) -> Set<MinTerm> {
  return mt.reduce(Set<MinTerm>()) { (a,m) -> Set<MinTerm> in
    var res: Set<MinTerm> = []
    for j in mt { if let x = tryMerge(m,j) { res.insert(x) } }
    return a.union(res.isEmpty ? [m] : res)
  }
}