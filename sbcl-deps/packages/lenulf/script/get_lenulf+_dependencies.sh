#!/bin/bash

# Stop on error
set -e

mkdir -p deps

cd deps

# Clone the ptb2cf repository.
git clone https://github.com/yosihide/ptb2cf
cd ptb2cf
git checkout 9374e8f49b6b3286df1ddc7451edc63d9ba54f16 # The commit this was tested with
cd ..

ln -s ./ptb2cf ~/quicklisp/local-projects/ptb2cf

cd ..

