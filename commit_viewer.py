#%%
import sys
import os
from git import Repo
import json
import readline
import readMethods
from pydoc import pipepager
from pprint import pprint

PATH = os.getcwd()
SRC = os.path.relpath("src/")
REPO = Repo(SRC)
assert not REPO.bare
INVALID_INPUT = True


def getFile(filename):
    try:
        handle  = open(filename,'r')
        return handle
    except FileNotFoundError:
        raise FileNotFoundError

def display(commit_dict):
    assert commit_dict

    keys = [key for key in commit_dict]

    for i,key in enumerate(keys):
        header = '\033[' + '2;33;40m' + 'Parent -- ' + key + "\n"
        for commit in commit_dict[key]:
            pipepager(header + REPO.git.show('--color=always',commit,'--',key), cmd='less -R')
#%%
def start() :
    #%%
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

    try:
        call = getattr(readMethods,"read_" + commit_list.split("_")[0])
        commit_dict = call(commit_FH)
    except(AttributeError):
        print("Method does not exist", file=sys.stderr)
        INVALID_INPUT = True
        return
    except(ValueError):
        INVALID_INPUT = True
        print("File Handle Error",file=sys.stderr)
        return
    else:
        commit_FH.close()

    display(commit_dict)

    INVALID_INPUT = False
    return

while(INVALID_INPUT):
    start()
