from collections import defaultdict
import csv

def setBit(bitset,num):
    mask = 1<<num
    bitset |= mask
    return bitset

comma = ", "

with open("callgraph-allfiles.txt") as cgfile:
    cgs = cgfile.readlines()
with open("pledge-table.csv") as ptfile:
    ptable = ptfile.read().splitlines()

function_callee_dict = defaultdict(list)
function_promise_dict = defaultdict(set)
syscall_promise_dict = defaultdict(set)
promise_list = []

function_name = "ERROR"
for line in cgs:
    if not line.startswith(" "): #Top level functions
        function_name = line.split()[0]
    else:
        callee = line.split()[0]
        function_callee_dict[function_name].append(callee)


for row in ptable:
    promise = row.split(",")[0]
    promise_list.append(promise)
    for syscall in row.split(", ")[1:]:
        syscall_promise_dict[syscall].add(promise)

# Get overlaps
# overlaps = [promises for _,promises in syscall_promise_dict.items() if len(promises) > 1]

for function, callees in function_callee_dict.items():
    for callee in callees:
        if callee in syscall_promise_dict:
            [function_promise_dict[function].add(promise) for promise in syscall_promise_dict[callee]]

with open("function_promises_bitvectors.inc","w") as fpfile:
    writer = csv.writer(fpfile)
    for key, value in function_promise_dict.items():
        bitset = 0
        for promise in value:
            bitset = setBit(bitset=bitset, num=promise_list.index(promise))
        # LIBC(functionName, promises, binary/handler)
        print("LIBC(" + key[:-2] + comma + ", ".join(value) + comma + bin(bitset)+ ")", file=fpfile)


with open("promises.inc","w") as incfile:
    writer = csv.writer(fpfile)

    for i, promise in enumerate(promise_list):
        if promise == 'unix':
            promise = 'unix_promise'
        print("PROMISE(" + promise + comma + "\"" + promise + "\"" + comma + str(bin(setBit(0,i)))  + ")",file=incfile)
