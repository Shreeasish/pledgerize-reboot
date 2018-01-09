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

    print(commit_dict)

    keys = [key for key in commit_dict]

    # for i in range(len(keys)): BEACUSE PYTHON..
    i = 0
    while i < len(commit_dict):
        print("\nNew Key\n")
        key = keys[i]
        commits = commit_dict[keys[i]]
        header = '\033[' + '2;33;40m' + 'Parent -- ' + key + "\n"

        # for commit in commit_dict[keys[i]]:
        # for i2 in range(len(commits)):
        j=0
        while (j < len(commits)):
            # Added key.split for modifications_commit.json. TODO Standardize filename:lineNumber seperator and Rerun
            print(j,len(commits))

            print(header + REPO.git.show('--color=always',commits[j],'--', key.split(",")[0]))
            command = input("\nInteresting: i \t Not Interesting: o \nNext: j \t Prev: k \t NextKey: n \t PrevKey: m \t ShowAgain: s \t Pipe to less: l -- ")
            print("\n")

            if command.lower() == 'i':
                pass
            
            elif command.lower() == 'o':
                pass
            
            elif command.lower() == 'j':
                j += 1

            elif command.lower() == 'k':
                if( j == 0 ):
                    print("\n FIRST COMMIT\n")
                    continue
                j -= 1
            
            elif command.lower() == 'n':
                break
            
            elif command.lower() == 'm':
                if(i == 0 ):
                    print("\n FIRST KEY \n")
                    continue
                i -= 2
            elif command.lower() == 's':
                print(header + REPO.git.show('--color=always',commits[j],'--', key.split(",")[0]))
            elif command.lower() == 'l':
                pipepager(header + REPO.git.show('--color=always',commits[j],'--', key.split(",")[0]),cmd="less -R")
            else:
                print("woops!")
        i += 1

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
        print("Incorrect File Type\n", file=sys.stderr )
        INVALID_INPUT = True
        return
    try:
        commit_FH = getFile(commit_list)
    except FileNotFoundError:
        print("File not Found\n", file=sys.stderr )
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
