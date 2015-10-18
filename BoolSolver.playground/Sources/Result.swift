public enum Result<T,E> { case Some(T), Error(E) }

extension Result : CustomStringConvertible {
  public func map<U>(f: T -> U) -> Result<U,E> {
    switch self {
    case let .Error(e): return .Error(e)
    case let x?: return .Some(f(x))
    }
  }
  public func flatMap<U>(f: T -> Result<U,E>) -> Result<U,E> {
    switch self {
    case let .Error(e): return .Error(e)
    case let x?: return f(x)
    }
  }
  public var description: String {
    switch self {
    case let s?: return String(reflecting: s)
    case let .Error(e): return String(reflecting: e)
    }
  }
}

public enum ParseError : ErrorType, CustomStringConvertible {
  case UnBal(String)
  case UnRec(Character)
  public var description: String {
    switch self {
    case let .UnBal(s): return s
    case let .UnRec(c): return String(c)
    }
  }
}
