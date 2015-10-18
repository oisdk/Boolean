extension Expr: BooleanLiteralConvertible {
  public init(booleanLiteral value: Bool) {
    self.init(value)
  }
}

extension Expr: StringLiteralConvertible {
  public init(extendedGraphemeClusterLiteral value: String) {
    self.init(value)
  }
  public init(stringLiteral value: String) {
    self.init(value)
  }
  public init(unicodeScalarLiteral value: String) {
    self.init(value)
  }
}

public prefix func !(b: Expr) -> Expr {
  return Expr(NOT: b)
}
public func &&(lhs: Expr, rhs: Expr) -> Expr {
  return Expr(lhs, AND: rhs)
}
public func ||(lhs: Expr, rhs: Expr) -> Expr {
  return Expr(lhs, OR: rhs)
}
public func ^ (lhs: Expr, rhs: Expr) -> Expr {
  return Expr(lhs, XOR: rhs)
}