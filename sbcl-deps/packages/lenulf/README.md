# Len's Preliminary ULF parser

This is an SBCL-ported and ASDF packaged version of Len's preliminary tree-to-ULF parser.


## Running the parser

If you would like to use this directly, without asdf or quicklisp, similar to
the original unpackaged system, you can use `init.lisp` and go into the
`lenulf` package.
```
* (load "init")
[loading messages....]
* (in-package :lenulf)

#<PACKAGE "LENULF">
* (english-to-ulf "This is a sentence")
[processing messages...]
(THIS.D~1 (IS.AUX~2 (A.D~3 SENTENCE.N~4)))
```

I would recommend making this accessible to quicklisp (by adding a symbolic
link to this directory in the quicklisp local projects directory--default is
`~/quicklisp/local-projects`). Then you can do the following.
```
* (ql:quickload :lenulf)
* (in-package :lenulf)
* (english-to-ulf "This is a sentence")
[processing messages...]
(THIS.D~1 (IS.AUX~2 (A.D~3 SENTENCE.N~4)))
```
The processing messages can be removed by setting the `*show-stages*` parameter
to `nil`.
```
* (ql:quickload :lenulf)
* (in-package :lenulf)
* (setf *show-stages* nil)
* (english-to-ulf "This is a sentence")

(THIS.D~1 (IS.AUX~2 (A.D~3 SENTENCE.N~4)))
```
If you would rather stay in your current package, the following code results in
the same input/output from your current package by using the global `*package*`
variable. NB: `intern-symbols-recursive` is from a separate utility package
[gute](https://github.com/genelkim/gute) (previously called
[cl-util](https://github.com/genelkim/cl-util)).
```
* (ql:quickload :gute)
* (ql:quickload :lenulf)
* (gute:intern-symbols-recursive ; intern output symbols to current package
    (let ((*package* (find-package :lenulf)) ; locally set *package* variable so symbols
                                             ; are interned as expected in lenulf
          (lenulf::*show-stages* nil)) ; locally turn off stage printing
      (lenulf:english-to-ulf "This is a sentence"))
    *package*) ; second argument of intern-symbols-recursive

(THIS.D~1 (IS.AUX~2 (A.D~3 SENTENCE.N~4)))
```
If your current package is locked (e.g. the `common-lisp` package in SBCL) you
may need to unlock it. This is common lisp implementation dependent. For SBCL
`(sb-ext:unlock-package *package*)` will do it.

## Choosing the underlying syntactic parser

The basic system and by default this uses the BLLIP parser with the parser path
set to the location where it is installed in the URCS grad network. To use this
elsewhere, you can modify the `*parser*` and `*pdata*` parameters in
`parse.lisp` to the appropriate locations.

This repository includes an extended version of the system, `:lenulf+`, under
the same package name, `:lenulf` which supports additional parsers. The separate
system allows the basic functionality to be available without downloading or
installing dependencies that are only used in the additional parsers. Once those
dependencies have been appropriately downloaded the extended system can be loaded
up with
```
* (ql:quickload :lenulf+)
* (in-package :lenulf)
```
To get the system to load, please get the
[ptb2cf](https://github.com/yosihide/ptb2cf.git) repository available on
quicklisp. The bash script `script/get_lenulf+_dependencies.sh` will set this
all up for the default quicklisp installation.  The other CL dependency,
`py4cl` will be downloaded automatically. If you lready had `lenulf` in your
quicklisp local projects directory before the `:lenulf+`, you may need to run
the following command to enable quicklisp to find the system.
```
* (ql:register-local-projects)
```
Quicklisp automatically rescans for systems when a change is made to the local
projects directory, but not when changes are made to its subdirectories. 

The API is the same as the basic package. The syntactic parser is chosen using
the `:synparser` keyword argument of `english-to-ulf` which can be `"BLLIP"`,
`"K&K"`, or `"K&M"`, case-insensitive.
- `"BLLIP"` is [the Charniak parser](https://github.com/BLLIP/bllip-parser)
- `"K&K"` is the [Kitaev and Klein self-attentive parser](https://github.com/nikitakit/self-attentive-parser).
- `"K&M"` is the [Kato and Matsubara gap parser](https://github.com/yosihide/ptb2cf) which is built on top of the K&K parser.
The K&K parser and _especially_ the K&M parser will take a while on the first
call since the model needs to be loaded into memory.

The following instructions describe how to install the K&K and K&M parsers,
which are Python systems.

### Installing the Python parsers

The K&K parser is called in Lisp with Python calls through `py4cl`. This
package assumes that the parser is already installed, that is the `benepar`
package can be imported from Python. [The repository](https://github.com/nikitakit/self-attentive-parser)
has the basic instructions for installing this parser. Here are a few 
issues that I ran into while installing the parser.

- You must use Python 3.6. The repository states that it is supported by Python 3.6+, but uses a keyword that became reserved in Python starting with 3.7 and will lead to an error.
- The `pip install benepar[cpu]` will install the most recent Tensorflow version, but the code is written for Tensorflow 1.x. So after this, please run
```
pip uninstall tensorflow
pip install tensorflow==1.15
```

The K&M parser is a bit more work to get working. It is a specially trained
version of the K&K parser as well as a separate Lisp pacakge. Please run the
script, `script/get_km_dependencies.sh` to download and decompress the
pretrained model and get the necessary dependencies. The model is several
gigabytes in size so the script will take a while to complete. In addition to
the K&K parser's dependencies, install
- pytorch
- pytorch-pretrained-bert

If you run out of space while pytorch-pretrained-bert is downloading the BERT
model, you'll need to delete the cache in `~/.pytorch-pretrained-bert/` before
rerunning with more space. This might happen on the URCS cluster since this
model and the necessary packages are all quite large and the cluster allots
a fairly limited amount of space for each person's home directory.

## standardize-ulf

This repository contains an additional separate package called `standardize-ulf`
which is complementary to the main `lenulf` package. It exports a function called
`standardize-ulf` which takes a ULF result (without token indices) from the `lenulf`
parser and infers a ULF formula which follows the ULF annotation guidelines. For 
example, the generic `ADV` suffix from `lenulf` is converted to `ADV-A` or `ADV-S`
according to the context. This standardization doesn't retain some of the ambiguity
inherent in the parses and makes an arbitrary call in some instances. This is not
important for converting to English, but may have unintended consequences if used
as the basis for inference. `standardize-ulf` takes a keyword argument `:pkg` which
is the package which the output symbols will be interned into. Below is an example
of its usage.
```
* (ql:quickload :lenulf)
* (ql:quickload :standardize-ulf)
* (in-package :lenulf)
* (use-package :standardize-ulf)
* (standardize-ulf (remove-token-indices (english-to-ulf "This is a sentence")) :pkg :lenulf)
(THIS.D ((PRES BE.V) (= (A.D SENTENCE.N))))
```

Getting this system working may require the following call to make Quicklisp aware
of this additional package in the repository.
```
* (ql:register-local-projects)
```

### standardize-ulf dependencies

The function `standardize-ulf` is placed in a separate package since it has additional
dependencies that are not necessary for the parser alone. The following dependencies
are not currently available automatically through quicklisp and must be installed in
the local-projects directory.
- [cl-util](https://github.com/genelkim/cl-util)
- [ttt](https://github.com/genelkim/ttt)
- [ulf-lib](https://github.com/genelkim/ulf-lib)
- [ulf2english](https://github.com/genelkim/ulf2english)
`ulf2english` requires its own install, so please take a look in the repository README for
instructions. 

## Original README by Len
```
          PARSING ENGLISH INTO ULF -- PRELIMINARY VERSION
          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Simplest usage: in allegro CL, do
  (load "init.lisp")
This will print out some guiding comments.

This is based on the idea that Treebank parses give pretty good POS
assignments. So along with parenteses (ignoring phrase labels), this
comes pretty close to ULF (using POS's to determine ULF atom types).

It does require quite a lot of parse-tree postprocessing (thus, tree
PRE-processing rules, before extracting a ULF), but this can be done
very modularly with rules written in TT -- a simplification of TTT,
using position indices in output templates to refer to pieces of an
input (generalizing the matching in Eta). See the rules in
  "preprocessing-rules.lisp".

After doing
  (load "init.lisp"),
the "english-to-ulf' function can be used (see "english-to-ulf.lisp" file)

The 'english-to-parse-tree' and 'parse-tree-to-ulf' functions can also
be used separately..

Initially I used sample *.cmb files from the Brown corpus for development.

I want to add quite a few more rules to "preprocessing-rules.lisp" (NB:
these also need to be added to "preprocess-tree-for-ulf", with attention
to ordering -- not all orderings work!)

I've made some adaptations so that basic BLLIP (Charniak) parses work
as well, but much checking remains to be done, ad in particular, TRACES
(empty constituents corresponding to "moved" phrases) need to be added.)

The lexical-level ULF derivation code is in
   pos+word-to-ulf.lisp
This also requires loading of "stem.lisp" (a variant of previous versions)

The tree transduction code is in "tt.lisp". There's a documentation file
for tt, viz., "tt-documantation". It's pretty simple!

At the time of writing (Sep 20/20), ULF postprocessing remains to be done
(based on looking at lots of examples of the "raw" ULF deriven from
Brown trees). It's not always clear what should be done in proeprocessing
parse trees or postprocessing ULFs.

It may also be quite feasible to rewrite the ULF-to-ELF rules in TT.
```
