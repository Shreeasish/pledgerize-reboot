import os
from collections import namedtuple

linesAST = [line.strip() for line in open("ids.flattened")]
headersAST = linesAST[0].split('|:')
FlatAST = namedtuple('FlatAST', headersAST)
listAST = [tokens.split('|:') for tokens in linesAST[1:]]
dictAST = {row[0] : FlatAST(*row) for row in listAST}

def printConjunct(conjunctID):
    print(dictAST[conjunctID].aststring, end='')

def printDisjunct(disjunct):
    conjuncts = [id.strip() for id in disjunct.split(',')]
    for conjunct in conjuncts:
        if conjunct.startswith('-'):
            print("NEG{", end='')
            printConjunct(conjunct)
            print("}", end='')
        else:
            print("   {", end='')
            printConjunct(conjunct)
            print("}", end='')
        print(" AND ", end="")
    print("")

printDisjunct("16763, 16874, 16912, 16627")



