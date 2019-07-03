import os
from collections import namedtuple

binExprList = [line.strip() for line in open('ids.binary')]
headersBin = binExprList[0]
BinNode = namedtuple('BinNode', headersBin)
binExprList = [tokens.split(',') for tokens in binExprList[1:]]
binDict = {row[0] : BinNode(*row) for row in binExprList}

leafExprList = [line.strip() for line in open("ids.leaves")]
headersLeaf = leafExprList[0].split('|:')
print(headersLeaf)
LeafNode = namedtuple('LeafNode', headersLeaf)
leafExprList = [tokens.split('|:') for tokens in leafExprList[1:]]
leafDict = {row[0] : LeafNode(*row) for row in leafExprList}

def traverse(id):
    if id in binDict:
        binNode = binDict[id]
        traverse(binNode.left)
        print(binNode.opName, end=" ")
        traverse(binNode.right)
    else:
        leafNode = leafDict[id]
        #print(leafNode.string, end=' ')

def printDisjunct(conjunctIDs):
    conjuncts = [id for id in conjunctIDs.split(',')]
    for conjunct in conjuncts:
        if conjunct.startswith('-'):
            print("NEG[", end='')
            traverse(conjunct[1:])
            print("]", end='')
        else:
            print("   [", end='')
            traverse(conjunct)
            print("]", end='')
        print(" AND ", end="")

printDisjunct('-16398,-16407,16431,-16438,16907,16964,-16968')

