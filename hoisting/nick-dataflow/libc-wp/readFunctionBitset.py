#! /home/shreeasish/anaconda3/envs/gitwork/bin/python 
import pandas as pd
import sys

def bitsettoString(bitstring):
    if pd.isna(bitstring):
        return "NaN"
    bitstring = bitstring[::-1]
    with open("pledge-list") as plistf:
        plist = plistf.read().splitlines()
    promise_list = list()
    for i,bit in enumerate(bitstring):
        if int(bit) == 1:
            promise_list.append(plist[i])
        else:
            promise_list.append("---")
    return " ".join(promise_list)

if len(sys.argv) <= 1:
    exit()

with open(sys.argv[-1]) as webfile:
    webfpledges = pd.read_csv(webfile, header=None, names=['function','long','bitstring'])

# with open("function-bitsets") as truthfile:
#     fpledges = pd.read_csv(truthfile, header=None, names=['function','long','bitstring'])

# jointpledges = webfpledges.join(fpledges, how='inner', lsuffix='_web', rsuffix='_truth')
#
# subset = jointpledges[['function_truth','bitstring_truth','bitstring_web']]

for rows in webfpledges.iterrows():
    print(rows[1].get('function'))
    print(bitsettoString(rows[1].get('bitstring').strip()))
