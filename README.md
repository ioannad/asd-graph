
# ASD-GRAPH

This small utility uses graphviz dot to visualise the dependencies
declared in a Common Lisp `<package-name>.asd` file.

## Requirements

* Linux OS

* Graphviz (dot should be in `/usr/bin/dot`)

* A Common Lisp REPL (CCL or SBCL tested)

* Quicklisp

## Usage

Load this file in a Common Lisp REPL (tested with CCL and SBCL), 
and if `<file-name>` is your path (including the path to the file)
to your `<package-name>.asd`, then evaluate:

`(asd-graph <file-name>)`

or

`(asd-graph <file-name> :output-dir <output-dir>)`

This will create a dot and a pdf file in the same directory as 
`<file-name>` or in `<output-dir>` respectively. 

If you prefer a different output (png, svg, `<etc>`), then run from 
the same directory as your .dot file, in a terminal, the following command:

`dot -T<etc> <package-name>.dot -o <output-file-name>`

where `<etc>` is the ending of your preferred format.

### Formats supported by dot:

canon, dot, fig, gd, gif, hpgl, imap, cmap, mif, mp, pcl, pic,
plain (ascii), png, ps, ps2, svg, vrml, vtx, wbmp.

According to the graphviz dot manual: 
http://www.graphviz.org/Documentation/dotguide.pdf
