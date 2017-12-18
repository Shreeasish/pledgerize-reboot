#%%
import os
from git import Repo

#%%
PATH = os.path.abspath("/home/ska196/Pledges/references/src")
REPO = Repo(PATH)
assert not REPO.bare
#For testing use 0:8
lines = [line for line in REPO.git.g('pledge').split('\n')[0:8]]

#%%
pledge_files = []
temp_group = []
for line in lines:
    temp_group.append(line)
    if len(line) == 0:
        pledge_files.append(temp_group[:-1])
        temp_group = []

#%%
# How do I add multiple items in a single dict comprehension
pledge_files_dict = {item[0]: x.split(":")[0] for item in pledge_files for x in item[1:] }
print(pledge_files_dict)