This directory contains vole code used to test the compiler and virtual machine. Code
files that starts with "failc_" are expected to fail to compile. Likewise, files that
start with "failx_" are expected to fail to execute.

During the testing process, several files are created using the same base file name.
   .volec is the vole code file (from a successful compile)
   .dot is the parse tree created by the compiler
   .compile_log is a copy of the compiler output
   .results_actual is the virtual machine output from running the code
   .results_expected (optional input) is a gold file used to verify actual runtime results

