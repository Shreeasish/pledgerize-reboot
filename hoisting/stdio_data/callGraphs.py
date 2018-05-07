from collections import defaultdict
import csv
from pprint import pprint


function_callee_dict = defaultdict(list)
function_promise_dict = defaultdict(set)
syscall_promise_dict = defaultdict(set)
pledge_list = list()

def setBit(bitset,num):
    mask = 1<<num
    bitset |= mask
    return bitset

with open("pledge-table2.csv") as ptfile:
    ptable = ptfile.read().splitlines()
with open("pledge_list") as plist:
    [pledge_list.append(row) for row in plist.read().splitlines()]
with open("stdio-callgraph") as cgfile:
    cgs = cgfile.readlines()


comma = ", "

edge_dict = defaultdict(set)
function_stack = []
depth = 0
try:
    for line in cgs:
        newdepth = line.count("|")
        function = line.split("|")[-1].split()[0]
        if newdepth == depth:
            # print(line)
            if function_stack:
                function_stack.pop()
            if function_stack:
                # edge_dict[function_stack[-1]].append(function) # For lists
                edge_dict[function_stack[-1]].add(function)
            # edge_dict[function].append([])
            if function not in edge_dict:
                # edge_dict[function] = [] # For lists
                edge_dict[function] = set()
            function_stack.append(function)
        elif depth < newdepth:
            # print(line)
            # edge_dict[function_stack[-1]].append(function) # For lists
            edge_dict[function_stack[-1]].add(function)
            if function not in edge_dict:
                # edge_dict[function] = [] # For lists
                edge_dict[function] = set()
            # edge_dict[function].append([]) # Make a key in dict
            function_stack.append(function)
        else:
            for _ in range(depth - newdepth + 1):
                popped = function_stack.pop()
            if function_stack:
                # edge_dict[function_stack[-1]].append(function) # For lists
                edge_dict[function_stack[-1]].add(function)
            if function not in edge_dict:
                # edge_dict[function] = [] # For lists
                edge_dict[function] = set()
            function_stack.append(function)
        depth = newdepth
except:
    print("----------------LINE-----------")
    print(line)
    print("----------------LINE-----------")
    print(e)

for row in ptable:
    syscall = row.split(",")[0] + "()"
    pledges =  row.split(",")[1:-1]
    [syscall_promise_dict[syscall].add(pledge) for pledge in pledges]

for function, callees in edge_dict.items():
    for callee in callees:
        if callee in syscall_promise_dict:
            [function_promise_dict[function].add(promise) for promise in syscall_promise_dict[callee]]

function_promise_dict["__sclose()"]


with open("function-promises-test","w") as fpfile:
    writer = csv.writer(fpfile)
    for key,functions in edge_dict.items():
        promiseSet = set()
        graphStack  = [key]
        path = []

        # if key == "__sclose()":
        #     import pdb; pdb.set_trace()

        for function in functions:
            graphStack.append(function)

        while (len(graphStack) > 0):
            popped = graphStack.pop()
            if popped in path:
                print("popped " + popped)
                continue
            path.append(popped)
            if function_promise_dict[popped] != set():
                [promiseSet.add(promise) for promise in function_promise_dict[popped]]
            for edge in edge_dict[popped]:
                graphStack.append(edge)
        print(path)
        bitset = 0
        for promise in promiseSet:
            bitset = setBit(bitset=bitset, num=pledge_list.index(promise))
        print("LIBC(" + key[:-2] + ", " + ", ".join(promiseSet) + ")", file=fpfile)
        # print("LIBC(" + key[:-2] + comma + str(bitset)+ ")", file=fpfile)

with open("stdio-bitset","w") as fpfile:
    writer = csv.writer(fpfile)
    for key,functions in edge_dict.items():
        promiseSet = set()
        graphStack  = [key]
        path = []

        # if key == "__sclose()":
        #     import pdb; pdb.set_trace()

        for function in functions:
            graphStack.append(function)

        while (len(graphStack) > 0):
            popped = graphStack.pop()
            if popped in path:
                print("popped " + popped)
                continue
            path.append(popped)
            if function_promise_dict[popped] != set():
                [promiseSet.add(promise) for promise in function_promise_dict[popped]]
            for edge in edge_dict[popped]:
                graphStack.append(edge)
        print(path)
        bitset = 0
        for promise in promiseSet:
            bitset = setBit(bitset=bitset, num=pledge_list.index(promise))
        # print("LIBC(" + key[:-2] + ", " + ", ".join(promiseSet) + ")", file=fpfile)
        print("libCHandlers.emplace(\"" + key[:-2] +"\"" + comma + str(bitset)+ ");",file=fpfile)





'''
Old Code
# with open("libC-emplace-stdio","w") as fpfile:
#     writer = csv.writer(fpfile)
#     for key, value in function_promise_dict.items():
#         bitset = 0
#         for promise in value:
#             bitset = setBit(bitset=bitset, num=pledge_list.index(promise))
#         # LIBC(functionName, promises, binary/handler)
#         # print("LIBC(" + key[:-2] + comma + ", ".join(value) + comma + str(bitset)+ ")", file=fpfile)
#         # libCHandlers.emplace("__srefill", 8);
#         print("libCHandlers.emplace(\"" + key[:-2] +"\"" + comma + str(bitset)+ ");",file=fpfile)


# with open("promises.inc","w") as incfile:
#     writer = csv.writer(fpfile)
#     for i, promise in enumerate(pledge_list):
#         # if promise == 'unix':
#         #     promise = 'unix_promise'
#         print("PROMISE(" + promise + comma + "\"" + promise + "\"" + comma + str(setBit(0,i))  + ")",file=incfile)
'''
