extension Expr: BooleanLiteralConvertible {
  public init(booleanLiteral value: Bool) {
    self = .Const(value)
  }
}

extension Expr: StringLiteralConvertible {
  public init(extendedGraphemeClusterLiteral value: String) {
    self = .Var(value)
  }
  public init(stringLiteral value: String) {
    self = .Var(value)
  }
  public init(unicodeScalarLiteral value: String) {
    self = .Var(value)
  }
}

public prefix func !(b: Expr) -> Expr {
  return Expr.NOT(b).simplified
}

public func &&(lhs: Expr, rhs: Expr) -> Expr {
  return Expr.AND(lhs, rhs).simplified
}

public func ||(lhs: Expr, rhs: Expr) -> Expr {
  return Expr.OR(lhs, rhs).simplified
}

public func ^ (lhs: Expr, rhs: Expr) -> Expr {
  return lhs && !rhs || !lhs && rhs
}

infix operator !& { associativity left precedence 120 }

public func !&(lhs: Expr, rhs: Expr) -> Expr {
  return !(lhs && rhs)
}

infix operator !| { associativity left precedence 115 }

public func !|(lhs: Expr, rhs: Expr) -> Expr {
  return !(lhs || rhs)
}

infix operator !^ { associativity left precedence 115 }

public func !^(lhs: Expr, rhs: Expr) -> Expr {
  return !(lhs ^ rhs)
}