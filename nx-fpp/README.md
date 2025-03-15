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
>>>>>>> devel

This directory contains products from an independent development 
of a FPP parser in the (Guile) Scheme language.

<<<<<<< HEAD
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
=======
## Example

The following shows a representative fpp file (from the math-component demo), 
and the resulting output from `nxfpp`:

MathSender.fpp:
```
module MathModule {

  @ Component for sending a math operation
  active component MathSender {

    # ----------------------------------------------------------------------
    # General ports
    # ----------------------------------------------------------------------

    @ Port for sending the operation request
    output port mathOpOut: OpRequest

    @ Port for receiving the result
    async input port mathResultIn: MathResult

    # ----------------------------------------------------------------------
    # Special ports
    # ----------------------------------------------------------------------

    @ Command receive port
    command recv port cmdIn

    @ Command registration port
    command reg port cmdRegOut

    @ Command response port
    command resp port cmdResponseOut

    @ Event port
    event port eventOut

    @ Telemetry port
    telemetry port tlmOut

    @ Text event port
    text event port textEventOut

    @ Time get port
    time get port timeGetOut

    # ----------------------------------------------------------------------
    # Commands
    # ----------------------------------------------------------------------

    @ Do a math operation
    async command DO_MATH(
                           val1: F32 @< The first operand
                           op: MathOp @< The operation
                           val2: F32 @< The second operand
                         )

    # ----------------------------------------------------------------------
    # Events
    # ----------------------------------------------------------------------

    @ Math command received
    event COMMAND_RECV(
                        val1: F32 @< The first operand
                        op: MathOp @< The operation
                        val2: F32 @< The second operand
                      ) \
      severity activity low \
      format "Math command received: {f} {} {f}"

    @ Received math result
    event RESULT(
                  result: F32 @< The math result
                ) \
      severity activity high \
      format "Math result is {f}"

    # ----------------------------------------------------------------------
    # Telemetry
    # ----------------------------------------------------------------------

    @ The first value
    telemetry VAL1: F32

    @ The operation
    telemetry OP: MathOp

    @ The second value
    telemetry VAL2: F32

    @ The result
    telemetry RESULT: F32

  }

}
```

output from `nxfpp path/to/MathSender.fpp`:
```
(trans-unit
 (module-defn
  (ident "MathModule")
  (seq (component-defn
        (@ (anno "Component for sending a math operation"))
        (ident "MathSender")
        (kind "active")
        (seq (output-port
              (@ (anno "Port for sending the operation request"))
              (ident "mathOpOut")
              (qual-ident "OpRequest"))
             (input-port
              (@ (anno "Port for receiving the result"))
              (ident "mathResultIn")
              (kind "async")
              (qual-ident "MathResult"))
             (port (@ (anno "Command receive port"))
                   (ident "cmdIn")
                   (spc-kind "command-receive"))
             (port (@ (anno "Command registration port"))
                   (ident "cmdRegOut")
                   (spc-kind "command-reg"))
             (port (@ (anno "Command response port"))
                   (ident "cmdResponseOut")
                   (spc-kind "command-resp"))
             (port (@ (anno "Event port"))
                   (ident "eventOut")
                   (spc-kind "event"))
             (port (@ (anno "Telemetry port"))
                   (ident "tlmOut")
                   (spc-kind "telemetry"))
             (port (@ (anno "Text event port"))
                   (ident "textEventOut")
                   (spc-kind "text-event"))
             (port (@ (anno "Time get port"))
                   (ident "timeGetOut")
                   (spc-kind "time-get"))
             (command
              (@ (anno "Do a math operation"))
              (ident "DO_MATH")
              (kind "async")
              (seq (param (@ (anno "The first operand"))
                          (ident "val1")
                          (type-name "F32"))
                   (param (@ (anno "The operation"))
                          (ident "op")
                          (type-name (qual-ident "MathOp")))
                   (param (@ (anno "The second operand"))
                          (ident "val2")
                          (type-name "F32"))))
             (event-spec
              (@ (anno "Math command received"))
              (ident "COMMAND_RECV")
              (seq (param (@ (anno "The first operand"))
                          (ident "val1")
                          (type-name "F32"))
                   (param (@ (anno "The operation"))
                          (ident "op")
                          (type-name (qual-ident "MathOp")))
                   (param (@ (anno "The second operand"))
                          (ident "val2")
                          (type-name "F32")))
              (severity "activity-low")
              (format (string "Math command received: {f} {} {f}")))
             (event-spec
              (@ (anno "Received math result"))
              (ident "RESULT")
              (seq (param (@ (anno "The math result"))
                          (ident "result")
                          (type-name "F32")))
              (severity "activity-high")
              (format (string "Math result is {f}")))
             (telemetry
              (@ (anno "The first value"))
              (ident "VAL1")
              (type-name "F32"))
             (telemetry
              (@ (anno "The operation"))
              (ident "OP")
              (type-name (qual-ident "MathOp")))
             (telemetry
              (@ (anno "The second value"))
              (ident "VAL2")
              (type-name "F32"))
             (telemetry
              (@ (anno "The result"))
              (ident "RESULT")
              (type-name "F32")))))))
```
