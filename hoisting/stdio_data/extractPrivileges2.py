from collections import defaultdict
import csv

def setBit(bitset,num):
    mask = 1<<num
    bitset |= mask
    return bitset

comma = ", "

pledge_list = set()

with open("callgraph-allfiles.txt") as cgfile:
    cgs = cgfile.readlines()
with open("pledge-table2.csv") as ptfile:
    ptable = ptfile.read().splitlines()
with open("pledge_list") as plist:
    [pledge_list.add(row) for row in plist.read().splitlines()]

pledge_list = list(pledge_list)


pledge_list

function_callee_dict = defaultdict(list)
function_promise_dict = defaultdict(set)
syscall_promise_dict = defaultdict(set)


function_name = "ERROR"



for line in cgs:
    if not line.startswith(" "): #Top level functions
        function_name = line.split()[0]
    else:
        callee = line.split()[0]
        function_callee_dict[function_name].append(callee)

syscall_promise_dict = defaultdict(set)


for row in ptable:
    syscall = row.split(",")[0] + "()"
    pledges =  row.split(",")[1:-1]
    [syscall_promise_dict[syscall].add(pledge) for pledge in pledges]

for function, callees in function_callee_dict.items():
    for callee in callees:
        if callee in syscall_promise_dict:
            [function_promise_dict[function].add(promise) for promise in syscall_promise_dict[callee]]

with open("function_bitvectors.inc","w") as fpfile:
    writer = csv.writer(fpfile)
    for key, value in function_promise_dict.items():
        bitset = 0
        for promise in value:
            bitset = setBit(bitset=bitset, num=pledge_list.index(promise))
        # LIBC(functionName, promises, binary/handler)
        # print("LIBC(" + key[:-2] + comma + ", ".join(value) + comma + str(bitset)+ ")", file=fpfile)
        print("LIBC(" + key[:-2] + comma + str(bitset)+ ")", file=fpfile)

with open("promises.inc","w") as incfile:
    writer = csv.writer(fpfile)

    for i, promise in enumerate(pledge_list):
        if promise == 'unix':
            promise = 'unix_promise'
        print("PROMISE(" + promise + comma + "\"" + promise + "\"" + comma + str(setBit(0,i))  + ")",file=incfile)
