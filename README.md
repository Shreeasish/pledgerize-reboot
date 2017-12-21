# <center>¯\\\_(ツ)\_/¯</center>

# Configs
* #####  Allow extended regular expressions 
    `git config --global grep.extendRegexp true`
* #####  Always include line numbers <br>
    `git config --global grep.lineNumber true`
* #####  Group output like ack!
    `git config --global alias.g "grep --break --heading --line-number"`


Source [Search a git repo like a ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja/)

# Requirements
* [GitPython](https://github.com/gitpython-developers/GitPython)

# Links
* [General Documentation of Progress](https://docs.google.com/document/d/1VXyJoxYt7o5XYmYGgnslxCLMEDZLoXGFu2RJJPBCMwI/edit?usp=sharing)

# Files in repo
* #### `interesting_files.txt` is the stdout for mine_git.py. 
    Has information about 
    1. Files which have multiple pledges

    2. Files which have multiple pledges inserted at different times

    3. Files which have no commits for pledges but somehow showed up in the grep

* #### `pledge_locations.txt` holds the grep output
* #### `commits.txt` has the five 5 previous commits to a pledge/tame insertion, grouped by file name.

