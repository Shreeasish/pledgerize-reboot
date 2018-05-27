from collections import defaultdict

with open("pledge-maybe.csv") as ptfile:
    ptable = ptfile.read().splitlines()

funcMap = defaultdict(set)

for line in ptable:
    for func in line.split(",")[1:]:
        # print(line.split(",")[0])
        funcMap[func[:-2]].add(line.split(",")[0])

funcMap
#{"setuid", std::bitset<COUNT>().set(PLEDGE_ID)},
for func,pledges in funcMap.items():
    print("{\"" + func + "\",",end="")
    print("|".join(["std::bitset<COUNT>().set("+ pledge +")" for pledge in pledges]), end="},\n")
    # print("|".join("std::bitset<COUNT>().set(" + pledges +")"),end="")
