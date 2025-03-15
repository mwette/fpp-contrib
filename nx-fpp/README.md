# FPP Parser Development

This directory contains products from an independent development 
of a FPP parser in the (Guile) Scheme language.

## Introduction

The starting point for the parser development is the file
`fpp-mach.scm`.  In this file we created a YACC (or Bison) like 
grammar for the FPP language.   

The script `mach.gen` will process the `fpp-mach.scm` file with
the NYACC compiler-compiler to produce the files `mach.d/fpp-tab.scm`
and `mach.d/fpp-act.scm`, which provide the parser machine tables
and the array of actions, respectively.   The grammar and machine
are created in a text form in `fpp-gram.txt`.  In addition, the NYACC
`lalr->bison` procedure is used to create a nominal Bison input 
file `fpp-gram.y`.

The `fpp-tab.scm` and `fpp-act.scm` files are included by 
`fpp-parser.scm`, with included lexical analyzer, to produce
an parser.  The parser procecure `read-fpp-file` reads a FPP
source file and generates an AST in the form of a s-expression
that is in a form called SXML which is comparable to an XML 
representation.  (In fact, a XML file can be generated using 
Guile's `sxml->xml` procedure.)

## Tools

The tool `nxfpp` will read a FPP source file and print out the
AST.a  The tool `fpp-tlm-help`, executed at the root of a project
with name of a deployment directory, will check the deployment
packets.xml file for missing or extra elements.   This assumes
at least the `fprime-util generate` build phase has completed
in order to provide the `locs.fpp` file.

That's all folks.
