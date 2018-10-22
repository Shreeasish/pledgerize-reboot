import pprint
from operator import itemgetter

# unsorted = [[('z', 0), ('f', 0), ('u', 0), ('c', 0), ('k', 0)],
#             [('h', 0), ('e', 0), ('l', 0), ('l', 0), ('o', 0)],
#             [('w', 0), ('o', 0), ('a', 0), ('a', 0), ('a', 0), ('h', 0)]]
# print(sorted(unsorted, key=lambda x: ''.join(l[0] for l in x), reverse=True))

disjunct1 = [[('1', 0), ('2', 1), ('6', 0), ('4', 1)]]
disjunct2 = [[('1', 0), ('2', 1), ('4', 0), ('7', 0)]]
disjunct3 = [[('9', 0), ('1', 1)]]

def merge(disjunct1, disjunct2):
    disjunct = disjunct1 + disjunct2
    return disjunct


pp = pprint.PrettyPrinter(indent=2)

def printDisjunct(disjunct):
    for conjunct in merge(disjunct1,disjunct2):
        pp.pprint(conjunct)


def sortDisjunct(disjunct):
    return sorted(disjunct, key=lambda x: int(''.join(tup[0] for tup in x)), reverse=False)



merged = merge(disjunct1, disjunct2)
merged = merge(merged, disjunct3)
final = sortDisjunct(merged)
pp.pprint(final)



