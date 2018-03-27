#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Clone the docker for sen2or and install it
Created on Wed Jun  7 13:54:58 2017

@author: Luigi Ranghetti <ranghetti.l @ irea.cnr.it>
"""

import os
import sys
import subprocess
import re
import platform
import urllib
import zipfile

def clone_repo(repository=["ranghetti",None]):
    import git
    # clone the repo
    git_dir = os.path.dirname(os.path.abspath(__file__)) + "/" + repository[1]
    if not os.path.exists(git_dir):
        os.makedirs(git_dir)
        repo = git.Repo.clone_from("http://github.com/"+repository[0]+"/"+repository[1], git_dir)
        
def download_repo(repository=["ranghetti",None], ref="master"):
    repo_url = 'https://github.com/'+repository[0]+'/'+repository[1]+'/archive/'+ref+'.zip'
    repo_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), repository[1])
    repo_zippath = repo_dir + '.zip'
    if not os.path.exists(repo_zippath):
        urllib.urlretrieve(repo_url, repo_zippath)
    if not os.path.exists(repo_dir):
        repo_zip = zipfile.ZipFile(repo_zippath, 'r')
        repo_zip.extractall(os.path.dirname(repo_dir))
        repo_zip.close()
        os.rename(os.path.join(os.path.dirname(repo_dir),repository[1]+"-"+ref), repo_dir)
        os.remove(repo_zippath)

def download_sen2cor(path=None):
    
    # set path and url
    if path is None:
        sen2cor_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), r"sen2cor")
    if not os.path.exists(sen2cor_dir):
        os.makedirs(sen2cor_dir)
    os = platform.system()
    if os == "Linux":
        sen2cor_url = "http://step.esa.int/thirdparties/sen2cor/2.4.0/Sen2Cor-2.4.0-Linux64.run"
    elif os == "Windows":
        sen2cor_url = "http://step.esa.int/thirdparties/sen2cor/2.4.0/Sen2Cor-2.4.0-win64.zip"
    else:
        sys.exit("Installing sen2cor on" + os + " was not yet implemented.")
    sen2cor_installer = os.path.join(sen2cor_dir, sen2cor_url.split("/")[-1])

    # download, extract and delete archive
    sen2cor_file = urllib.URLopener()
    sen2cor_file.retrieve(sen2cor_url, sen2cor_installer)
    if os == "Linux":
        curr_dir = os.getcwd()
        os.chdir(sen2cor_dir)
        os.chmod(sen2cor_installer, 0755)
        subprocess.call("/bin/bash ./" + sen2cor_url.split("/")[-1] + " --quiet --nox11 --target ./", shell=True)
        os.remove(sen2cor_installer)
        os.chdir(curr_dir)
        sen2cor_bin = os.path.join(sen2cor_dir,"bin","L2A_Process")
    elif os == "Windows":
        sen2cor_zip = zipfile.ZipFile(sen2cor_installer, 'r')
        sen2cor_zip.extractall(sen2cor_dir)
        os.remove(sen2cor_installer)
        sen2cor_bin = os.path.join(sen2cor_dir,"bin","L2A_Process.bat")
    
    # return sen2cor executable path
    return sen2cor_bin

#def clone_sen2cor_docker(default_dir=None):
#
#    # clone the repo
#    clone_repo(["ranghetti","sen2cor_docker"])
#    git_docker_dir = os.path.dirname(os.path.abspath(__file__)) + "/sen2cor_docker"
#
#    # change the directory of in/out products
#    if default_dir is None:
#        default_dir = git_docker_dir + "/data/"
#    default_dir = os.path.join(default_dir, '')
#    yml_file = git_docker_dir + "/sen2cor.yml"
#    with open(yml_file, "r") as sources:
#        lines = sources.readlines()
#    with open(yml_file, "w") as sources:
#        for line in lines:
#            sources.write(re.sub(r'~/Documents/Sentinel/2/', default_dir, line))
#    
#    # change the sen2cor version
#    dockerfile = git_docker_dir + "/Dockerfile"
#    with open(dockerfile, "r") as sources:
#        lines = sources.readlines()
#    with open(dockerfile, "w") as sources:
#        for line in lines:
#            sources.write(re.sub(r'SEN2COR_VERSION=\'2.3.0\'', 'SEN2COR_VERSION=\'2.3.1\'', line))
#    
#    # create in/out directories
#    print(default_dir)
#    if not os.path.exists(default_dir):
#        os.makedirs(default_dir)
#    if not os.path.exists(default_dir + "archives/"):
#        os.makedirs(default_dir + "archives/")
#    if not os.path.exists(default_dir + "unzipped_scenes/"):
#        os.makedirs(default_dir + "unzipped_scenes/")
#    default_dir_file = open(git_docker_dir + "/default_data_dir.txt", 'w+')
#    default_dir_file.write(default_dir)
#    default_dir_file.close()
#
#    
#def build_sen2cor_docker(default_dir=None):
#    
#    git_docker_dir = os.path.dirname(os.path.abspath(__file__)) + "/sen2cor_docker"
#    
#    # build the docker
#    os.chdir(git_docker_dir) # necessary because ./build cannot be run by another directory
#    build_cmd = "./build"
#    try:
#        retcode = subprocess.call(build_cmd, shell=True)
#        if retcode < 0:
#            print("Error: " + repr(-retcode))
##        else:
##            print("Sen2cor docker correctly installed")
#            
#    except OSError as e:
#        print("System error:" + e)
        
if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Clone and install the docker for sen2cor")
    parser.add_argument("-d", "--default_dir", help="Path of default directory in which processing L1C products", default=None)
#    parser.add_argument("-b", "--build", help="Build sen2cor (root necessary)", default=False, action="store_true")
#    parser.add_argument("-i", "--install", help="Install sen2cor files", default=False, action="store_true")
    options = parser.parse_args()
    clone_repo(["ranghetti","fetchLandsatSentinelFromGoogleCloud"])
    clone_repo(["ranghetti","Sentinel-download"])
#    if options.build is False and options.install is False:
#        options.build = True
#        options.install = True
#    if options.install:
#        clone_sen2cor_docker(options.default_dir)
#    if options.build:
#        build_sen2cor_docker(options.default_dir)
    clone_sen2cor_docker(options.default_dir)
    download_sen2cor(options.default_dir)

