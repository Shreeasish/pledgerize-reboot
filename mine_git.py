#%%
import sys
import os
from git import Repo
import json
#%%
PATH = os.path.relpath("src/")
REPO = Repo(PATH)
assert not REPO.bare
# For testing use 0:62

print("Grep pledges...\n",file=sys.stderr)

lines = [line for line in REPO.git.g('if.(pledge').split('\n')]
# lines = [line for line in REPO.git.g('if.(pledge').split('\n')[0:62]]

#%%
print("Grouping Pledges...\n",file=sys.stderr)

pledge_files = []
temp_group = []
for line in lines:
    temp_group.append(line)
    if len(line) == 0:
        pledge_files.append(temp_group[:-1])
        temp_group = []

#%%
# pledge_files_dict = {item[0]: x.split(":")[0] for item in pledge_files for x in item[1:] }
print("Building filename:line number dictionary...\n",file=sys.stderr)

multiple_pledge_list = []

pledge_files_dict = {}
for items in pledge_files:
    for line_number in items[1:]:
        if items[0] not in pledge_files_dict:
            pledge_files_dict[items[0]] = [line_number.split(":")[0]]
        else:
            pledge_files_dict[items[0]].append( line_number.split(":")[0] )
    if len(pledge_files_dict[items[0]]) > 2:

        multiple_pledge_list.append({ "file": items[0], "lines": pledge_files_dict[items[0]] })

        print(" {} : Multiple pledges \n Line Numbers {} \n".\
        format(items[0], pledge_files_dict[items[0]]),file=sys.stdout)

        print(" {} : Multiple pledges \n Line Numbers {} \n".\
        format(items[0], pledge_files_dict[items[0]]),file=sys.stderr)
with open("multipledge_files.json","w") as multipledge_files_FH:
    json.dump(multiple_pledge_list,multipledge_files_FH)
#%%
# Write to file
# Write as JSON

# pledge_locations_file = open('pledge_locations.txt','w')
# for key in pledge_files_dict:
#     print(key,file=pledge_locations_file)
#     print(pledge_files_dict[key],file=pledge_locations_file)
#     print("\n",file=pledge_locations_file)
# pledge_locations_file.close()

pledge_loc_list = [{"name":k,"line" : v} for k,v in pledge_files_dict.items()]
with open("pledge_locations.json","w") as pledge_locations_FH:
    json.dump(pledge_loc_list, pledge_locations_FH)

'''
# pledges seem to always be inserted in two lines like this
# +	if (tame("stdio rpath", NULL) == -1)
# +		err(1, "tame");
# git log -L 216,217,bin/md5/md5.c
'''

#%%
# {filename: (commit_id1,commit_id2) }
# Should only have one commit id per filename the ones which have two or more
# are more interesting
print("Retrieving git log for each line... \n",file=sys.stderr)
file_commit_dict = {}
modification_list = []
additive_pledge_list = []
# Add check for line numbers which have been modified more than once
# Usually there will always be more than one change since the name was changed from tame
for file_name in pledge_files_dict:
    commit_set = set()
    line_numbers = pledge_files_dict[file_name]

    for idx in range(0,len(line_numbers),1):
        modification_ids = []
        '''
        Get the last commit shown (earliest commit in git log for the line number).
        This should be the commit where it was inserted.
        '''
        final_commit = ""
        for line in REPO.git.log( '-L ' + line_numbers[idx] + "," + line_numbers[idx] + ":" + file_name ).split("\n"):

            print("Getting logs for lines -- " + line_numbers[idx] + "," + line_numbers[idx] + ":" + file_name,file=sys.stderr)

            # Picks up the word commit in the commit messages, ignored by a single check as commit messages
            # begin with white space
            if "commit" in line:
                final_commit = line.split(" ")[1]
                if(len(final_commit) > 1):
                    modification_ids.append(final_commit)

        if(len(modification_ids) > 1):
            modification_list.append( \
            {"id":"{},{}".format( file_name, line_numbers[idx] ), "commit_ids":modification_ids } \
             )
            # print("{}:{}::{}".format(file_name, line_numbers[idx], modification_ids),file=modification_FH)

        if(len(final_commit) > 1):
            commit_set.add(final_commit)

    if(len(commit_set) > 1):
        additive_pledge_list.append(\
        {"file":file_name, "commit_ids": list(commit_set)} \
        )

    if file_name in file_commit_dict:
        print("File {} reoccurs in dict".format(file_name),file=sys.stdout)
    else:
        file_commit_dict[file_name] = commit_set

with open("modifications.json","w") as modification_FH:
    json.dump(modification_list, modification_FH)
with open("additive_pledges.json", "w") as additive_pledges_FH:
    json.dump(additive_pledge_list, additive_pledges_FH)

'''
Get the commit window for each file
git log --pretty=%P -n 1 <child>
'''

#%%
print("Retrieving commit window...\n",file=sys.stderr)

# outfile = open("commits.txt","w")
# for file_name in file_commit_dict:
#     print(file_name,file=outfile)
#     for commit in file_commit_dict[file_name]:
#         print("For Commit: {}".format(commit),file=outfile)
#         if len(commit) == 0:
#             # zero commit issue
#             print("{} has no commits \n".format(file_name),file=sys.stdout)
#         else:
#             print(REPO.git.log('--pretty=%P', '-n', '5', commit),file=outfile)
# outfile.close()

commit_windows = [ {"parent": "{}, {}".format(file_name, commit),  "window": REPO.git.log('--pretty=%P', '-n', '5', commit).split("\n") }\
 for file_name, commits in file_commit_dict.items() for commit in commits]
with open("commit_windows.json","w") as commit_windows_FH:
    json.dump(commit_windows,commit_windows_FH)
