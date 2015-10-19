let not = !"A"

print("NOT:")
print(not)
print(not.truthTable + "\n")

let and = "A" && "B"

print("AND:")
print(and)
print(and.truthTable + "\n")

let nand = "A" !& "B"

print("NAND:")
print(nand)
print(nand.truthTable + "\n")

let nor = "A" !| "B"

print("NOR:")
print(nor)
print(nor.truthTable + "\n")

let xor = "A" ^ "B"

print("XOR:")
print(xor)
print(xor.truthTable + "\n")

let xnor = "A" !^ "B"

print("XNOR:")
print(xnor)
print(xnor.truthTable + "\n")

let multiplexer = "A" && !"S" || "B" && "S"

print("Multiplexer:")
print(multiplexer)
print(multiplexer.truthTable + "\n")

let fullAdderSum = "A" ^ "B" ^ "C"

print("Full adder sum:")
print(fullAdderSum)
print(fullAdderSum.truthTable + "\n")

let fullAdderCout = "A" && "B" || "C" && ("A" ^ "B")

print("Full adder Carry-out:")
print(fullAdderCout)
print(fullAdderCout.truthTable + "\n")