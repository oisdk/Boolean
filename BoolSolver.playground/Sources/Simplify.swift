public enum Expr : Equatable {
  case Const(Bool)
  case Var(String)
  indirect case NOT(Expr)
  indirect case AND(Expr,Expr)
  indirect case OR (Expr,Expr)
}

public func == (lhs: Expr, rhs: Expr) -> Bool {
  switch (lhs,rhs) {
  case let (.Const(l), .Const(r)  ): return l == r
  case let (.NOT (l),  .NOT  (r)  ): return l == r
  case let (.AND (a,b),.AND  (c,d)): return a==c && b==d || a==d && b==c
  case let (.OR  (a,b),.OR   (c,d)): return a==c && b==d || a==d && b==c
  case let (.Var(a),.Var(b)): return a == b
  default: return false
  }
}

public func < (lhs: Bool, rhs: Bool) -> Bool { return lhs && !rhs }
extension Bool : Comparable {}

extension Dictionary where Value : Equatable {
  func symmDiffOne(var with: [Key:Value]) -> [Key:Value]? {
    var count = 0
    for (k,v) in self {
      if let same = with[k] where same == v {
        with.removeValueForKey(k)
      } else if ++count > 1 {
        return nil
      }
    }
    guard with.count == 1 else { return nil }
    var res = self
    with.keys.forEach { k in res.removeValueForKey(k) }
    return res
  }
}

func tryInsert<K : Hashable, V : Equatable>(into: [[K:V]])(with: [K:V]) -> [[K:V]] {
  let res = into.flatMap(with.symmDiffOne)
  return res.isEmpty ? [with] : res
}

func minOnce(s: [[String:Bool]]) -> [[String:Bool]] {
  let minned =  s.flatMap(tryInsert(s))
  return minned.sort { (a,b) in
    a.lexicographicalCompare(b) { (c,d) in c.0 == d.0 ? (c.1 && !d.1) : c.0 < d.0 }
  }
}

extension Expr {
  var primeImpl: [[String:Bool]] {
    var mts = minTermsTrue
    for _ in 0..<10 {
      let next = minOnce(mts)
      if next.elementsEqual(mts, isEquivalent: {(a,b) in a.keys.sort() == b.keys.sort()}) { return next }
      mts = next
    }
    return mts
  }
}

func toExpr(t: (String, Bool)) -> Expr {
  return t.1 ? .Var(t.0) : .NOT(.Var(t.0))
}

func toAnd(var d: [String:Bool]) -> Expr {
  guard let f = d.popFirst() else { return true }
  return d.map(toExpr).reduce(toExpr(f), combine: Expr.AND)
}

func toOr(var a: [[String:Bool]]) -> Expr {
  guard let f = a.popLast() else { return false }
  return a.map(toAnd).reduce(toAnd(f), combine: Expr.OR)
}

extension Expr {
  public var simplified: Expr {
    return toOr(primeImpl)
  }
}

