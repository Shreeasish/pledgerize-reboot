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

with open("pledge-maybe.csv") as ptfile2:
    pmaybe = ptfile2.read().splitlines()

'''
with open("stdio-callgraph") as cgfile:
    cgs = cgfile.read().splitlines()
with open("stdlib-callgraph") as cgfile2:
    cgs = cgs + cgfile2.read().splitlines()
'''

with open("arch-amd64-gdtoa-callgraph") as cgfile:
    cgs = cgfile.read().splitlines()
with open("arch-amd64-gen-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("asr-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("citrus-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("compat-43-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("crypt-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("db-btree-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("db-db-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("db-hash-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("db-mpool-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("db-recno-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("dlfcn-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("gdtoa-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("gen-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("gmon-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("hash-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("libc-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("locale-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("net-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nls-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("quad-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("regex-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("rpc-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("softfloat-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("stdio-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("stdlib-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("string-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("sys-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("termios-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("thread-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("time-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("uuid-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("yp-callgraph") as cgfile:
    cgs = cgs + cgfile.read().splitlines()

comma = ", "

edge_dict = defaultdict(set)
function_stack = []
toplevelfunctions = set()
found_functions = set()
depth = 0
try:
    for line in cgs:
        newdepth = line.count("|")
        function = line.split("|")[-1].split()[0]

        if line.endswith(':'):
            if function.endswith('():'):
                function = function[:-1]
            found_functions.add(function)

        if(newdepth == 0):
            toplevelfunctions.add(function)

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


del cgs

for row in pmaybe:
    promise = row.split(",")[0]
    for syscall in row.split(",")[1:]:
        syscall_promise_dict[syscall].add(promise)

syscall_promise_dict

for function, callees in edge_dict.items():
    for callee in callees:
        if callee in syscall_promise_dict:
            [function_promise_dict[function].add(promise) for promise in syscall_promise_dict[callee]]

with open("libc-promises-maybe","w") as fpfile:
    writer = csv.writer(fpfile)
    for key,functions in edge_dict.items():
        promiseSet = set()
        graphStack  = [key]
        path = []

        for function in functions:
            graphStack.append(function)

        while (len(graphStack) > 0):

            popped = graphStack.pop()
            path.append(popped)

            if function_promise_dict[popped] != set():
                [promiseSet.add(promise) for promise in function_promise_dict[popped]]

            if syscall_promise_dict[popped] != set():
                [promiseSet.add(promise) for promise in syscall_promise_dict[popped]]

            for edge in edge_dict[popped]:
                if edge in path:
                    continue
                graphStack.append(edge)
        print( key[:-2] + ", " + ", ".join(promiseSet), file=fpfile)
