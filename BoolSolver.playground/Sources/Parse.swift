extension String.CharacterView {
  private func divide(@noescape isC: Character throws -> Bool)
    rethrows -> (String.CharacterView,String.CharacterView)? {
      guard let i = try indexOf(isC) else { return nil }
      return (prefixUpTo(i),suffixFrom(i.successor()))
  }
  private func brks(open: Character, _ close: Character)
    -> Result<(String.CharacterView, String.CharacterView),ParseError> {
      var count = 1
      return dropFirst().divide{ c in
        if c == close { --count } else if c == open  { ++count }
        return count == 0
      }.map(Result.Some) ?? .Error(.UnBal(String(self)))
  }
}

extension Character {
  var opEquiv: Result<(Expr,Expr) -> Expr,ParseError> {
    switch self {
    case "∧": return .Some(Expr.AND)
    case "∨": return .Some(Expr.OR)
    case "⊼": return .Some(Expr.NAND)
    case "⊽": return .Some(Expr.NOR)
    case "⊻": return .Some(Expr.XOR)
    default : return .Error(.UnRec(self))
    }
  }
}

extension String.CharacterView {
  public var asExp: Result<Expr,ParseError> {
    switch first {
    case " "?: return dropFirst().asExp
    case "("?:
      return brks("(", ")").flatMap { (a,b) in
        a.asExp.flatMap { fExp in
          guard let i = b.indexOf({ c in c != " " }) else {
            return .Some(fExp)
          }
          return b[i].opEquiv.flatMap { op in
            if i == self.endIndex { return .Error(.UnBal(String(self))) }
            return b.suffixFrom(i.successor()).asExp.map {
              bExp in op(fExp,bExp)
            }
          }
        }
      }
    default:
      for i in indices {
        if let op = self[i].opEquiv { return opAt(i, op: op) }
      }
      return .Some(Expr.Var(String(self)))
    }
  }
  private func opAt(i: Index, op: (Expr,Expr) -> Expr) -> Result<Expr,ParseError> {
    return prefixUpTo(i).asExp.flatMap { fExp in
      if i == self.endIndex { return .Error(.UnBal(String(self))) }
      return self.suffixFrom(i.successor()).asExp.map { bExp in
        op(fExp,bExp)
      }
    }
  }
}
