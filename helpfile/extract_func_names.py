# -*- coding: utf-8 -*-
"""
Extract function definitions from indicated files

@author Patrick O'Keeffe <pokeeffe@wsu.edu

Created Thu Feb 7 2012
"""

import sys


if __name__ == "__main__":
    filelist = sys.argv[1:]
    if not filelist:
        filelist = [raw_input('Specify file: ')]
    if not filelist: sys.exit(0)

    results = open('found_funcs.txt', 'w')
    results.write('Extracted functions \n-------------------\n')    
    
    for each in filelist:
        if not each.endswith('.ipf'):
            print ("File '%s' is not an Igor Pro function file. Skipping..."
                    % each)
        print ("Extracting function definitions from '%s' ... " % each),
        num_found = 0
        ffile = open(each, 'r')
        results.write('\n' + each + '\n' + ('-'*len(each)) + '\n')
        for line in ffile:
            if (line.startswith('Function') or line.startswith('ThreadSafe')):
                num_found += 1
                line = line.replace('Function/DF ','').replace('Function/C ','')
                line = line.replace('Function/S ','').replace('Function/WAVE ','')
                results.write(line.replace('Function ','').replace('ThreadSafe ',''))
        print "found %u functions" % num_found
    raw_input('Press any key to continue...')


