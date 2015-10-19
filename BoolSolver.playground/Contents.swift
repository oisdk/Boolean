let multiplexer = "A" && !"S" || "B" && "S"

print(multiplexer.truthTable + "\n")

let fullAdderSum = "A" ^ "B" ^ "C"

print(fullAdderSum.truthTable + "\n")

let fullAdderCout = ("A" && "B" || "C" && ("A" ^ "B"))

print(fullAdderCout.truthTable)
