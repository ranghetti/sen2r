# deprecated (use the docker)

import subprocess
import re
import os
import sys
import argparse

# Charge functions
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
from find_executable import find_executable

def callSen2Cor(inputimg, verbose=False, overwrite=False):
    
    l2aPath = find_executable("L2A_Process")
    # print l2aPath
    # print inputimg
    if l2aPath is not None:
        cmd = l2aPath + " " + inputimg
    else:
        cmd = "L2A_Process " + inputimg
    
    # print cmd

    try:
        retcode = subprocess.call(cmd, shell=True)
        if retcode < 0:
            print "Error: " + `-retcode`
        else:
            print "Sen2Cor correctly runned"
    except OSError as e:
        print "System error:" + e

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Correct with Sen2Cor")
    parser.add_argument("inputimg", help="Path of input L1C image")
    parser.add_argument("--overwrite", help="Overwrite files if existing locally", default=False)
    parser.add_argument("-v", "--verbose", help="Show download status", action="store_true", default=False)
    options = parser.parse_args()
    callSen2Cor(options.inputimg, options.verbose, options.overwrite)


