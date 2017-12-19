#%%
import os
from git import Repo

#%%
PATH = os.path.abspath("/home/ska196/Pledges/references/src")
REPO = Repo(PATH)
assert not REPO.bare
# For testing use 0:62
lines = [line for line in REPO.git.g('pledge').split('\n')[0:62]]

#%%
pledge_files = []
temp_group = []
for line in lines:
    temp_group.append(line)
    if len(line) == 0:
        pledge_files.append(temp_group[:-1])
        temp_group = []

#%%
# pledge_files_dict = {item[0]: x.split(":")[0] for item in pledge_files for x in item[1:] }
pledge_files_dict = {}
for items in pledge_files:
    for line_number in items[1:]:
        if items[0] not in pledge_files_dict:
            pledge_files_dict[items[0]] = [line_number.split(":")[0]]
        else:
            pledge_files_dict[items[0]].append( line_number.split(":")[0] )
print(pledge_files_dict)

#%%
'''
pledges seem to always be inserted in two lines like this
+	if (tame("stdio rpath", NULL) == -1)
+		err(1, "tame");
git log -L 216,217,bin/md5/md5.c
'''

for file_name in pledge_files_dict:
    commit_set = set()
    line_numbers = pledge_files_dict[file_name]
    for idx in range(0,len(line_numbers),2):
        print(line_numbers[idx] + "," + line_numbers[idx+1] + ":" + file_name)
        '''
        Get the last commit shown (earliest commit in for the line number). 
        This should be the commit where it was inserted.
        '''
        final_commit = ""
        for line in REPO.git.log( '-L ' + line_numbers[idx] + "," + line_numbers[idx+1] + ":" + file_name ).split("\n"):
            if "commit" in line:
                final_commit = line.split(" ")[1]
        commit_set.add(final_commit)
    print("COMMIT SET -- ", commit_set)
