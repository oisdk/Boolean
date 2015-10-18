let multiplexer = ("A" && !"S") || ("B" && "S")

print(multiplexer.truthTable + "\n")

let fullAdderSum = Expr(Expr("A", XOR: "B"), XOR: "C")

print(fullAdderSum.truthTable + "\n")

let fullAdderCout = ("A" && "B") || ("C" && Expr("A", XOR: "B"))

print(fullAdderCout.truthTable)