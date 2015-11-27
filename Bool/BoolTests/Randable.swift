protocol Randable {
  static func rand(size: Int) -> Self
}

extension Int: Randable {
  
}