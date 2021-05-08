#!/bin/bash

# Stop on error
set -e

mkdir -p deps

cd deps

# Clone the repository with the code that we need to run.
git clone https://github.com/nikitakit/self-attentive-parser.git
cd self-attentive-parser
git checkout 1ee43a8f93d6f3259c09ea1ff57cf5124ec32efc # The commit I tested this with.
cd ..

# Download and decompress pretrained model.
wget -O model.tar https://www.dropbox.com/s/55wxplk70uqf0z2/model.tar?dl=1
tar xvf model.tar
rm model.tar

cd ..

