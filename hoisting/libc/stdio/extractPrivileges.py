from collections import defaultdict

with open("callgraph-allfiles.txt") as cgfile:
    cgs = cgfile.readlines()
with open("pledge-table.csv") as ptfile:
    ptable = ptfile.read().splitlines()

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

for row in ptable:
    promise = row.split(",")[0]
    for syscall in row.split(",")[1:]:
        syscall_promise_dict[syscall].add(promise)

# Get overlaps
# overlaps = [promises for _,promises in syscall_promise_dict.items() if len(promises) > 1]

test = []
for function, callees in function_callee_dict.items():
    for callee in callees:
        if callee in syscall_promise_dict:
            test.append(callee)








'''
End of File
'''
