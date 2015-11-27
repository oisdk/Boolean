extension SequenceType where Generator.Element : Hashable {
  internal var frequencies: [Generator.Element:Int] {
    var res: [Generator.Element:Int] = [:]
    for e in self { res[e] = (res[e] ?? 0) + 1 }
    return res
  }
  internal var mostFrequent: Generator.Element? {
    var freqs: [Generator.Element:Int] = [:]
    var g = generate()
    guard var be = g.next() else { return nil }
    var bc = 1
    freqs[be] = 1
    while let ne = g.next() {
      let nc = (freqs[ne] ?? 0) + 1
      if nc > bc { (be,bc) = (ne,nc) }
      freqs[ne] = nc
    }
    return be
  }
}

internal extension SequenceType {
  func reduce1(@noescape f: (Generator.Element,Generator.Element) -> Generator.Element) -> Generator.Element? {
    var g = generate()
    guard var x = g.next() else { return nil }
    for y in self { x = f(x,y) }
    return x
  }
  func partition<A,B>(@noescape f: Generator.Element -> Either<A,B>) -> ([A],[B]) {
    var (a,b): ([A],[B]) = ([],[])
    for e in self {
      switch f(e) {
      case let .Left (x): a.append(x)
      case let .Right(x): b.append(x)
      }
    }
    return (a,b)
  }
}

internal enum Either<A,B> { case Left(A), Right(B) }

func comparing<T,C : Comparable>(f: T -> C) -> (T,T) -> Bool {
  return { (a,b) in
    f(a) < f(b)
  }
}

func fst<A,B>(t: (A,B)) -> A { return t.0 }
func snd<A,B>(t: (A,B)) -> B { return t.1 }
