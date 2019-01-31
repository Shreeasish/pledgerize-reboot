with open("found_functions") as found_functionsf:
    ffunctions = found_functionsf.read().splitlines()

ffunctions= set(ffunctions)


with open("Symbols.list") as symbolsf:
    Symbols = symbolsf.read().splitlines()

Symbols = set(Symbols)


len(Symbols)
len(ffunctions)

rem = Symbols.difference(ffunctions)
