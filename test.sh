#!/bin/bash

function compile_tests() {
   for test_file in *.vole ; do
      #echo "Compiling" ${test_file} ;
      ../obj/volec ${test_file} ;
   done
}

function execute_tests() {
   for test_file in *.volec ; do
      #echo "Running" ${test_file} ;
      ../obj/volem ${test_file} Driver go 1 ;
   done
}

(
 cd language_tests;
 compile_tests;
 echo "---------------------------------------"
 execute_tests;
) | tee results.txt

