#!/usr/bin/env python
# -*- coding: utf-8 -*-
import glob
import os
import shutil

# As this is implemented as string substitution, these must be in order from
# longest to shortest.
LOOKUPS = [
    ('XXXLITERAL', {'d': 'd0', 's': 'e0'}),
    ('XXXPRINT', {'d': '1e25.15', 's': '1e15.7'}),
    ('XXXCAST', {'d': 'dble', 's': 'real'}),
    ('XXXCAP', {'d': 'DOUBLE PRECISION', 's': 'REAL'}),
    ('XXX', {'d': 'double precision', 's': 'real'}),
    ('XX', {'d': 'd', 's': 's'})
]

CWD = os.getcwd()
root_dir = os.path.dirname(CWD)
os.mkdir(root_dir + os.sep + "double")
os.mkdir(root_dir + os.sep + "double" + os.sep + "Examples")
os.mkdir(root_dir + os.sep + "double" + os.sep + "Examples" + os.sep + "Output")
os.mkdir(root_dir + os.sep + "single")
os.mkdir(root_dir + os.sep + "single" + os.sep + "Examples")
os.mkdir(root_dir + os.sep + "single" + os.sep + "Examples" + os.sep + "Output")

def expand_templates(data):
    """Expand the templated code in a list of strings ``data``. Changes
    ``data`` in-place.
    """
    for key, rule in LOOKUPS:
        for index, line in enumerate(data):
            if "XX" in line:
                if line[0] == 'c' and key == 'XX':
                    data[index] = line.replace(key, rule[prefix].upper())
                else:
                    data[index] = line.replace(key, rule[prefix])

# expand templates on the core Fortran files and the top-level Makefile.
templates_samenames = [CWD + os.sep + t
                       for t in ["Makefile", "printstat.F", "psecond.F", "stat.h"]]
templates = glob.glob(CWD + os.sep + "XX*.F") + templates_samenames
for folder, prefix in zip([root_dir + os.sep + t
                           for t in ["double", "single"]],
                          ['d', 's']):
    for template in templates:
        with open(template, 'r') as f:
            data = list(f)
        expand_templates(data)
        outname = folder + os.sep + os.path.basename(template).replace("XX", prefix)

        with open(outname, 'w+') as outfile:
            for line in data:
                outfile.write(line)

# expand templates in the examples directory.
example_templates = (glob.glob(CWD + os.sep + "Examples" + os.sep + "*.F") +
                     [CWD + os.sep + "Examples" + os.sep + "matvec.h"] +
                     [CWD + os.sep + "Examples" + os.sep + "mkmax.h"] +
                     [CWD + os.sep + "Examples" + os.sep + "Makefile"])
for folder, prefix in zip([root_dir + os.sep + t
                           for t in ["double", "single"]],
                          ['d', 's']):
    for template in example_templates:
        with open(template, 'r') as f:
            data = list(f)
        expand_templates(data)
        outname = folder + os.sep + "Examples" + os.sep +                      \
                  os.path.basename(template).replace("XX", prefix)

        with open(outname, 'w+') as outfile:
            for line in data:
                outfile.write(line)

# copy the data files.
for folder in ["double", "single"]:
    infiles = glob.glob(os.sep.join([CWD, "Data", folder, "ExampleData", "*"]))
    destination = os.sep.join([root_dir, folder, "Examples"])
    for infile in infiles:
        shutil.copy(infile, destination)

    outfiles = glob.glob(os.sep.join([CWD, "Data", folder, "Output", "*"]))
    destination = os.sep.join([root_dir, folder, "Examples", "Output"])
    for outfile in outfiles:
        shutil.copy(outfile, destination)
