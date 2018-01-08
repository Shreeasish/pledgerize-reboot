import json

def read_additive(fileHandle):
    if(fileHandle == None):
        raise ValueError
    # fileHandle = open('additive_pledges.json','r')
    data = json.load(fileHandle)
    commit_dict = dict()
    for json_obj in data[:2]:
        commit_dict[json_obj['file']] = json_obj['commit_ids']

    commit_dict
    return commit_dict
