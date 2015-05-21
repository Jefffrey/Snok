#!/usr/bin/python

import ninja_syntax
import itertools
import os
import fnmatch
import re
import sys
import argparse

# Gets all files from the root that match a specific
# regex pattern.
def get_files(root, pattern):
    pattern = fnmatch.translate(pattern)
    for dir, dirs, files in os.walk(root):
        for f in files:
            if re.match(pattern, f):
                yield os.path.join(dir, f)

# Generates the location in which to store the corresponding
# object file, for a .cpp source file.
def object_file(cpp):
    return os.path.join('obj', re.sub(r'\.cpp', '.o', cpp))

# Argument parsing
parser = argparse.ArgumentParser()
parser.add_argument('--cxx', default='clang++', metavar='executable', help='compiler name')
args = parser.parse_args()

# Ninja instance
ninja = ninja_syntax.Writer(open('build.ninja', 'w'))

# Ninja variables
ninja.variable('ninja_required_version', '1.5')
ninja.variable('builddir', 'obj')
ninja.variable('include_flags', '-Iinclude -Ideps/aid/include')
ninja.variable('compiler_flags', '-Wall -Wextra -Wfatal-errors -Werror -std=c++14')
ninja.variable('linker_flags', '-lSFML-graphics -lSFML-window -lSFML-system')
ninja.variable('compiler', args.cxx)

# Compilation rule
ninja.rule('cxx',
        command = '$compiler -c $compiler_flags $include_flags $in -o $out',
        description = 'C++ $in')

# Linking rule
ninja.rule('link',
        command = '$compiler $compiler_flags $linker_flags $in -o $out',
        description = 'LINK $in')

# Source files builds 
src_files = list(get_files('src', '*.cpp'))
obj_files = [object_file(cpp) for cpp in src_files]
for cpp in src_files:
    ninja.build(object_file(cpp), 'cxx', inputs = cpp)

# Snok executables build
client_objs = list(obj_files)
client_objs.remove('obj/src/server.o')

server_objs = list(obj_files)
server_objs.remove('obj/src/client.o')

ninja.build('client', 'link', inputs = client_objs)
ninja.build('server', 'link', inputs = server_objs)

# Default build
ninja.default(['client', 'server'])
