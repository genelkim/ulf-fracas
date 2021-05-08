#!/bin/bash

echo "Downloading Stanford CoreNLP jars..."

mkdir jars
cd jars
wget http://cs.rochester.edu/u/gkim21/data/stanford-corenlp-3.9.1.jar
wget http://cs.rochester.edu/u/gkim21/data/stanford-corenlp-3.9.1-models.jar

echo "Done!"

