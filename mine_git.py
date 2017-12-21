#%%
import sys
import os
from git import Repo

#%%
PATH = os.path.abspath("/home/ska196/Pledges/references/src")
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
pledge_files_dict = {}
for items in pledge_files:
    for line_number in items[1:]:
        if items[0] not in pledge_files_dict:
            pledge_files_dict[items[0]] = [line_number.split(":")[0]]
        else:
            pledge_files_dict[items[0]].append( line_number.split(":")[0] )
    
    if len(pledge_files_dict[items[0]]) > 2:
        print(" {} : Multiple pledges \n Line Numbers {} \n".\
        format(items[0], pledge_files_dict[items[0]]),file=sys.stdout)
        print(" {} : Multiple pledges \n Line Numbers {} \n".\
        format(items[0], pledge_files_dict[items[0]]),file=sys.stderr)


pledge_locations_file = open('pledge_locations.txt','w')
for key in pledge_files_dict:
    print(key,file=pledge_locations_file)
    print(pledge_files_dict[key],file=pledge_locations_file)
    print("\n",file=pledge_locations_file)
pledge_locations_file.close()


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
for file_name in pledge_files_dict:
    commit_set = set()
    line_numbers = pledge_files_dict[file_name]
    for idx in range(0,len(line_numbers),1):
        '''
        Get the last commit shown (earliest commit in for the line number). 
        This should be the commit where it was inserted.
        '''
        final_commit = ""
        for line in REPO.git.log( '-L ' + line_numbers[idx] + "," + line_numbers[idx] + ":" + file_name ).split("\n"):
            print("Getting logs for lines -- " + line_numbers[idx] + "," + line_numbers[idx] + ":" + file_name,file=sys.stderr)
            if "commit" in line:
                final_commit = line.split(" ")[1]
        commit_set.add(final_commit)
    
    if(len(commit_set) > 1):
        print("{} has pledges inserted at different times \
        \nCommit Ids -- {}".format(file_name,commit_set),file=sys.stdout)

    if file_name in file_commit_dict:
        print("File {} reoccurs in dict".format(file_name),file=sys.stdout)
    else:
        file_commit_dict[file_name] = commit_set

'''
Get the commit window for each file
git log --pretty=%P -n 1 <child>
'''

#%%
print("Retrieving commit window...\n",file=sys.stderr)
outfile = open("commits.txt","w")
for file_name in file_commit_dict:
    print(file_name,file=outfile)
    for commit in file_commit_dict[file_name]:
        print("For Commit: {}".format(commit),file=outfile)
        if len(commit) == 0:
            # zero commit issue
            print("{} has no commits \n".format(file_name),file=sys.stdout)
        else:
            print(REPO.git.log('--pretty=%P', '-n', '5', commit),file=outfile)
outfile.close()
