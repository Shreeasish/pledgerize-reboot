from collections import defaultdict
import csv
from pprint import pprint


with open("nostatic/arch-amd64-gdtoa-callgraph-no-static") as cgfile:
    cgs = cgfile.read().splitlines()
with open("nostatic/arch-amd64-gen-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/asr-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/citrus-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/compat-43-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/crypt-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/db-btree-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/db-db-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/db-hash-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/db-mpool-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/db-recno-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/dlfcn-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/gdtoa-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/gen-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/gmon-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/hash-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/libc-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/locale-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/net-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/nls-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/quad-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/regex-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/rpc-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/softfloat-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/stdio-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/stdlib-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/string-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/sys-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/termios-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/thread-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/time-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/uuid-callgraph-no-static") as cgfile:
    cgs = cgs + cgfile.read().splitlines()
with open("nostatic/yp-callgraph-no-static") as cgfile:
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


with open("non-static-edge-list","w") as nsedgelistf:
    [print(key,file=nsedgelistf) for key in edge_dict.keys()]
