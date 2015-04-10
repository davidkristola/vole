#!/bin/bash

ONE=1
number_of_tests=0
number_of_failures=0

function report_failure() { # $1 is action (compile or run), $2 is file
   echo
   echo "***"
   echo "*** FAILED $1 $2 ***"
   echo "***"
   echo
   let "number_of_failures += ONE"
}

function report_test() { # $1 is action (compile or run), $2 is file
   echo "Test $1 $2";
   let "number_of_tests += ONE"
}

function expected_compile_result() {
	if [ "${1:0:5}" == "failc" ] ;
	then
	   return 1
	else
	   return 0
	fi
}

function compile_one_test() {
   test_file="$1"
   test_type="compile"

   expected_compile_result ${test_file}
   expected_code=$?
   #echo "expected_code = <${expected_code}>"

   if [ ${expected_code} -ne 0 ] ;
   then
      test_type="fail-compile"
   fi
   report_test ${test_type} ${test_file}

   sans_ext=${test_file%.*}
   compile_log=${sans_ext}.compile_log

   ../obj/volec -v -vv ${test_file} > ${compile_log};
   return_code=$?

   #echo the return code is ${return_code}
   if [ ${return_code} -ne ${expected_code} ] ;
   then
      report_failure ${test_type} ${test_file};
   fi
}

function compile_tests() {
   for test_file in *.vole ; do
      compile_one_test ${test_file};
   done
}

function expected_execute_result() {
   if [ "${1:0:5}" == "failx" ] ;
   then
      return 1
   else
      return 0
   fi
}

function execute_one_test() {
   test_file="$1"
   test_type="execute"

   expected_execute_result ${test_file}
   expected_code=$?
   #echo "expected_code = <${expected_code}>"

   if [ ${expected_code} -ne 0 ] ;
   then
      test_type="fail-execute"
   fi
   report_test ${test_type} ${test_file}

   sans_ext=${test_file%.*}
   results_actual=${sans_ext}.results_actual
   results_expected=${sans_ext}.results_expected

   ../obj/volem ${test_file} Driver go 1 > ${results_actual} ;
   return_code=$?

   #echo the return code is ${return_code}
   if [ ${return_code} -ne ${expected_code} ] ;
   then
      report_failure ${test_type} ${test_file};
   fi
   if [ -e ${results_expected} ] ;
   then
       report_test compare ${test_file}
      cmp ${results_actual} ${results_expected}
      return_code=$?
      if [ ${return_code} -ne 0 ] ;
      then
         report_failure compare ${test_file};
      fi
   fi
}

function execute_tests() {
	for test_file in *.volec ; do
		execute_one_test ${test_file};
	done
}

(
 cd language_tests;
 rm *.volec
 rm *.dot
 rm *.results_actual
 rm *.compile_log
 compile_tests;
 execute_tests;

 echo
 echo "${number_of_tests} tests, ${number_of_failures} failures."
) | tee results.txt
