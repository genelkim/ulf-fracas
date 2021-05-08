# ULF-FraCaS

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Generating Natural Logic Inferences from Unscoped Episodic Logical Forms on the FraCaS dataset

## Installation

Before installing this package, make sure you have Anaconda, sbcl, and Java installed on your machine. sbcl can be installed by Anaconda if that is already available. Then install [Quicklisp](https://www.quicklisp.org/beta/). The SBCL dependecies are installed with the following commands.

```
> ./script/install-sbcl-dependencies.sh
```

### Python dependencies

The ULF2English package requires interfacing with a Python package. So please set up a conda environment. Python 2.7 or 3.6 is fine.
```
$ conda create -n ulf-fracas python=3.6
$ conda activate ulf-fracas
```

Then install the pattern package.
```
# For Python 3
$ pip install pattern
# For Python 2
$ ./script/install-pattern-en.sh
```

Additionally, please follow the installation instructions of the `lenulf` dependency for the k&k parser.

### Java dependencies

The Java dependencies only requires downloading a couple of jars.
```
$ cd dynamic-polarity
$ ./get-jars.sh
$ cd ..
```

### Running experiment from the paper

You can run the code on section 1 of the FraCaS dataset with the following steps.
```
$ sbcl
...[sbcl loading messages]...
* (ql:quickload :ulf-fracas)
* (in-package :ulf-fracas)
* (eval-gold-fracas)
```

This will take about 1 hour for a decent compuuter to run completely.

