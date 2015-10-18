public enum Expr : Equatable {
  case Const(Bool)
  case Var(String)
  indirect case NOT (Expr)
  indirect case AND (Expr, Expr)
  indirect case OR  (Expr, Expr)
  indirect case NAND(Expr, Expr)
  indirect case NOR (Expr, Expr)
  indirect case XOR (Expr, Expr)
  indirect case XNOR(Expr, Expr)
}

public func == (lhs: Expr, rhs: Expr) -> Bool {
  switch (lhs,rhs) {
  case let (.Const(l),.Const(r)  ): return l == r
  case let (.NOT (l),  .NOT (r)  ): return l == r
  case let (.AND (a,b),.AND (c,d)): return a == b && c == d
  case let (.OR  (a,b),.OR  (c,d)): return a == b && c == d
  case let (.NAND(a,b),.NAND(c,d)): return a == b && c == d
  case let (.NOR (a,b),.NOR (c,d)): return a == b && c == d
  case let (.XOR (a,b),.XOR (c,d)): return a == b && c == d
  case let (.XNOR(a,b),.XNOR(c,d)): return a == b && c == d
  case let (.Var(a),.Var(b)): return a == b
  default: return false
  }
}

private func simplifyOnce(e: Expr) -> Expr {
  switch e {
  case .Var: return e
  case .Const: return e
  case let .NOT(.Const(x)): return .Const(!x)
  case let .NOT(.NOT(x)): return simplifyOnce(x)
  case let .NOT(x): return .NOT(simplifyOnce(x))
  case let .AND(x,.Const(true)): return simplifyOnce(x)
  case let .AND(.Const(true),x): return simplifyOnce(x)
  case .AND(_, .Const(false)), .AND(.Const(false), _): return .Const(false)
  case let .AND(.OR(a,b),.OR(c,d)):
    let (w,x,y,z) = (simplifyOnce(a),simplifyOnce(b),simplifyOnce(c),simplifyOnce(d))
    if w == y { return .OR(w,.AND(x,z)) }
    if w == z { return .OR(w,.AND(y,z)) }
    if x == z { return .OR(x,.AND(y,w)) }
    return .AND(simplifyOnce(.OR(w,x)),simplifyOnce(.OR(y,z)))
  case let .AND(a,.OR(b,c)):
    let (x,y,z) = (simplifyOnce(a),simplifyOnce(b),simplifyOnce(c))
    if x == y || x == z { return x } else { return .AND(x,simplifyOnce(.OR(y,z))) }
  case let .AND(.OR(b,c),a):
    let (x,y,z) = (simplifyOnce(a),simplifyOnce(b),simplifyOnce(c))
    if x == y || x == z { return x } else { return .AND(x,.OR(y,z)) }
  case let .AND(a,b):
    let (x,y) = (simplifyOnce(a),simplifyOnce(b))
    if x == y { return x } else { return .AND(x,y) }
  case let .OR(x,.Const(false)): return simplifyOnce(x)
  case let .OR(.Const(false),x): return simplifyOnce(x)
  case .OR(_, .Const(true)),   .OR(.Const(true), _): return .Const(true)
  case let .OR(.AND(a,b),.AND(c,d)):
    let (w,x,y,z) = (simplifyOnce(a),simplifyOnce(b),simplifyOnce(c),simplifyOnce(d))
    if w == y { return .AND(w,.OR(x,z)) }
    if w == z { return .AND(w,.OR(y,x)) }
    if x == z { return .AND(x,.OR(y,w)) }
    return .OR(simplifyOnce(.AND(w,x)),simplifyOnce(.AND(y,z)))
  case let .OR(a,.AND(b,c)):
    let (x,y,z) = (simplifyOnce(a),simplifyOnce(b),simplifyOnce(c))
    if x == y || x == z { return x } else { return .OR(x,simplifyOnce(.AND(y,z))) }
  case let .OR(.AND(b,c),a):
    let (x,y,z) = (simplifyOnce(a),simplifyOnce(b),simplifyOnce(c))
    if x == y || x == z { return x } else { return .OR(x,simplifyOnce(.AND(y,z))) }
  case let .OR (a,b):
    let (x,y) = (simplifyOnce(a),simplifyOnce(b))
    if x == y { return x } else { return simplifyOnce(.OR(x,y)) }
  case let .NAND(l,r):
    let r = simplifyOnce(.AND(l,r))
    if case let .AND(x,y) = r {
      return .NAND(x,y)
    } else {
      return .NOT(r)
    }
  case let .NOR(l,r):
    let r = simplifyOnce(.OR(l,r))
    if case let .OR(x,y) = r {
      return .NOR(x,y)
    } else {
      return .NOT(r)
    }
  case let .XOR(l,r):
    let (x,y) = (simplifyOnce(l),simplifyOnce(r))
    let r = simplifyOnce(.AND(.OR(x,y),.OR(.NOT(x),.NOT(y))))
    if case let .AND(.OR(a,b),.OR(.NOT(c),.NOT(d))) = r where (a == c && b == d) || (a == d && b == c) {
      return .XOR(a,b)
    }
    if case let .OR(.AND(a,.NOT(b)),.AND(.NOT(c),d)) = r where a == c && b == d {
      return .XOR(a,b)
    }
    if case let .OR(.AND(.NOT(b),a),.AND(.NOT(c),d)) = r where a == c && b == d {
      return .XOR(a,b)
    }
    if case let .OR(.AND(.NOT(b),a),.AND(d,.NOT(c))) = r where a == c && b == d {
      return .XOR(a,b)
    }
    return r
  case let .XNOR(l,r):
    let r = simplifyOnce(.XOR(l,r))
    if case let .XOR(a,b) = r {
      return .XNOR(a,b)
    }
    return .NOT(r)
  }
}

extension Expr : CustomStringConvertible {
  public var description: String {
    switch self {
    case let .Const(b): return b ? "1" : "0"
    case let .NOT (e) : return "¬" + e.description
    case let AND (a,b): return "(" + a.description + "∧" + b.description + ")"
    case let OR  (a,b): return "(" + a.description + "∨" + b.description + ")"
    case let NAND(a,b): return "(" + a.description + "⊼" + b.description + ")"
    case let NOR (a,b): return "(" + a.description + "⊽" + b.description + ")"
    case let XOR (a,b): return "(" + a.description + "⊻" + b.description + ")"
    case let XNOR(a,b): return "¬(" + a.description + "⊻" + b.description + ")"
    case let .Var(s): return s
    }
  }
}

extension Expr {
  public var simplified: Expr {
    let s = simplifyOnce(self)
    return s == self ? s : s.simplified
  }
}

extension Expr {
  public func solve(v: String, val: Expr) -> Expr {
    switch self {
    case let .Var(s): return s == v ? val : self
    case .Const: return self
    case let .NOT (e) : return Expr.NOT (e.solve(v, val: val))
    case let AND (a,b): return Expr.AND (a.solve(v, val: val),b.solve(v, val: val)).simplified
    case let OR  (a,b): return Expr.OR  (a.solve(v, val: val),b.solve(v, val: val)).simplified
    case let NAND(a,b): return Expr.NAND(a.solve(v, val: val),b.solve(v, val: val)).simplified
    case let NOR (a,b): return Expr.NOR (a.solve(v, val: val),b.solve(v, val: val)).simplified
    case let XOR (a,b): return Expr.XOR (a.solve(v, val: val),b.solve(v, val: val)).simplified
    case let XNOR(a,b): return Expr.XNOR(a.solve(v, val: val),b.solve(v, val: val)).simplified
    }
  }
}




