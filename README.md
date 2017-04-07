
# ASD-GRAPH

This small utility uses graphviz dot to visualise the dependencies
declared in a Common Lisp `<system-name>.asd` file.

## Requirements

* Linux OS

* Graphviz (dot should be in `/usr/bin/dot`)

* A Common Lisp REPL (CCL or SBCL, tested)

* Quicklisp

## Usage

Load this file in a Common Lisp REPL (tested with CCL and SBCL), 
and if `<file-name>` is your path (including the path to the file)
to your `<system-name>.asd`, then evaluate:

`(asd-graph <file-name>)`

optional keywords:

`:output-dir <output-dir>` and `:format <fmt>`

This will create a dot and an svg (or `<fmt>` if keyword :format was used) file in the same
directory as `<file-name>`, or in `<output-dir>` if keyword
:output-dir was used.

### Formats supported by dot:

`<fmt>` can take one of the following values:

canon, dot, fig, gd, gif, hpgl, imap, cmap, mif, mp, pcl, pic,
plain (ascii), png, ps, ps2, svg, vrml, vtx, wbmp.

According to the graphviz dot manual: 
http://www.graphviz.org/Documentation/dotguide.pdf

### Supported .ASD syntax

For now, only the simplest <system-name>.asd syntax is supported,
e.g.:

```common-lisp
(defsystem "hello-lisp"
  :description "hello-lisp: a sample Lisp system."
  :version "0.0.1"
  :author "Joe User <joe@example.com>"
  :licence "Public Domain"
  :depends-on ("optima.ppcre" "command-line-arguments")
  :components ((:file "packages")
               (:file "macros" :depends-on ("packages"))
               (:file "hello" :depends-on ("macros"))))
```	       	      

This example is from the [ASDF Manual](https://common-lisp.net/project/asdf/asdf.html#The-defsystem-form)

This utility visualises the part after `:components`.

## Examples


* The well known [alexandria package](https://gitlab.common-lisp.net/alexandria/alexandria) for common-lisp

* My pet project, jeffrey, aka [CGraph, the Choiceless Grapher](https://github.com/ioannad/jeffrey)
