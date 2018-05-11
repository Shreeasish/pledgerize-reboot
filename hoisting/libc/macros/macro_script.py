with open("macro-calls","r") as usedmacrosf:
    usedmacros = usedmacrosf.read().splitlines()

macrodefs = open("nick-regex2","r").read()

finalmacros  =  [macro for macro in usedmacros if macro in macrodefs]

len(finalmacros)

with open("newFinalMacros","w") as finalmacrof:
    print("\n".join(finalmacros),file=finalmacrof)
