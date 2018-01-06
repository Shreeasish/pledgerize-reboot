#%%
import sys
import os
from git import Repo
import json
import readline
from pprint import pprint

PATH = os.getcwd()
INVALID_INPUT = True


def getFile(filename):
    try:
        handle  = open(filename,'r')
        return
    except FileNotFoundError:
        raise FileNotFoundError


#%%
def start() :
    global INVALID_INPUT
    json_files = [filename for filename in os.listdir(PATH) if filename.endswith('.json')]
    print(', '.join(json_files))
    

    def completer(text, state):
        options = [i for i in json_files if i.startswith(text)]
        if state < len(options):
            return options[state]
        else:
            return None

    readline.parse_and_bind("tab: complete")
    readline.set_completer(completer)
    
    commit_list = input("Select Commit List \n")

    if(commit_list == 'exit'):
        exit()
    
    if(commit_list.endswith('.json') != True):
        print("Incorrect File Type\n", File=sys.stderr )
        INVALID_INPUT = True
        return
    try:
        commit_FH = getFile(commit_list)
    except FileNotFoundError:
        print("File not Found\n", File=sys.stderr )
        return

    INVALID_INPUT = False
    return

while(INVALID_INPUT):
    start()


