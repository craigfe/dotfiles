#!/usr/bin/python

import os
import csv
import filecmp
import logging
import shutil

logging.basicConfig(
    level=logging.INFO,
    datefmt='%H:%M:%S',
    format='[%(asctime)s] %(levelname)-8s %(message)s'
)
lgr = logging.getLogger(__name__)
linksFilepath = "links.tsv"
projectDir =  os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def main():
    linkFiles()

def linkFiles():
    section = ''
    with open(linksFilepath) as linksFile:
        for line in csv.reader(decomment(linksFile), delimiter='\t'):

            # Create symlinks
            if len(line) < 2:
                lgr.warning("Unable to parse line: [" + ", ".join(line) + "]")
                continue

            target = os.path.join(projectDir, line[0])
            link_name = os.path.expanduser(line[1])

            file_already_exists = os.path.lexists(link_name)

            if file_already_exists and not paths_equal(link_name,target):
                move_into_repo(link_name, target)
                link(link_name, target)

            elif not file_already_exists:
                link(link_name, target)

def move_into_repo(replacement_path, destination_path):
    lgr.warning("A different file/directory already exists at " + replacement_path)

    if os.path.isdir(destination_path): 
        shutil.rmtree(destination_path)
    elif os.path.isfile(destination_path):
        os.remove(destination_path)

    shutil.move(replacement_path, destination_path)

def link(link_name, target):
    lgr.info(contractuser(link_name) + " => " + contractuser(target))

    " Create directories along the path to the link as necessary "
    parent_dir=os.path.dirname(link_name)
    if not os.path.exists(parent_dir):
        os.makedirs(parent_dir)

    os.symlink(target, link_name)


def decomment(csvfile):
    for row in csvfile:
        raw = row.split("#")[0].strip()
        if raw: yield raw

def paths_equal(path1, path2):
    " Compare two files, links and/or directories for equality recursively "
    if os.path.samefile(path1, path2):
        return True

    elif os.path.isfile(path1) and os.path.isfile(path2):
        return filecmp.cmp(path1, path2)

    elif os.path.isdir(path1) and os.path.isdir(path2):
        return dir_trees_equal(path1, path2)

    elif os.path.islink(path1) and os.path.islink(path2):
        return os.path.realpath(path1) == os.path.realpath(path2)

def dir_trees_equal(dir1, dir2):
    """
    Compare two directories recursively. Files in each directory are
    assumed to be equal if their names and contents are equal.

    @param dir1: First directory path
    @param dir2: Second directory path

    @return: True if the directory trees are the same and 
        there were no errors while accessing the directories or files, 
        False otherwise.
   """

    dirs_cmp = filecmp.dircmp(dir1, dir2)
    if len(dirs_cmp.left_only)>0 or len(dirs_cmp.right_only)>0 or \
        len(dirs_cmp.funny_files)>0:
        return False
    (_, mismatch, errors) =  filecmp.cmpfiles(
        dir1, dir2, dirs_cmp.common_files, shallow=False)
    if len(mismatch)>0 or len(errors)>0:
        return False
    for common_dir in dirs_cmp.common_dirs:
        new_dir1 = os.path.join(dir1, common_dir)
        new_dir2 = os.path.join(dir2, common_dir)
        if not are_dir_trees_equal(new_dir1, new_dir2):
            return False
    return True

def contractuser(path):
    """ This is the inverse of os.path.expanduser() """

    home_symbol = '~'
    home = os.path.expanduser(home_symbol)
    if path.startswith(home):
        return path.replace(home, home_symbol)
    return path


if __name__ == "__main__":
    main()


