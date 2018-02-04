#!/bin/bash
#rm vole*.ad[sb] ### can't do this because it deletes volea.adb and volem.adb

# this is invoked from the obj subdirectory
cd ..

if ( [ vole_lex.l -nt vole_lex_dfa.ads ] || [ vole.y -nt kv-avm-vole_parser.adb ] ) then
   echo "need to regenerate files"

   # delete the generated files
   rm vole_goto.ads
   rm vole_lex_dfa.adb
   rm vole_lex_dfa.ads
   rm vole_lex_io.adb
   rm vole_lex_io.ads
   rm vole_shift_reduce.ads
   rm vole_tokens.ads
   
   rm kv-avm-vole_parser*.ad[sb]
   rm kv-avm-vole_lex*.ad[sb]
   rm *.a
   echo "Counting lines of code in the non-generated files..."
   grep ";" *.ad[sb] | wc -l
   echo "generating the lexer..."
   aflex vole_lex.l
   echo "generating the parser..."
   ayacc "(vole.y, Debug => On)"
   gnatchop -w --GCC=/opt/gcc-4.9.1/bin/gcc *.a
   rm *.a
   mv kv-avm-vole_parser.adb kv-avm-vole_parser.adb-change_using_sed
   cat kv-avm-vole_parser.adb-change_using_sed | sed "s/DEBUG : constant boolean := TRUE;/DEBUG : boolean renames Verbose;/" > kv-avm-vole_parser.adb

else
   echo "generated files are up-to-date"
fi
cd obj
gnatmake -I.. volec
#cd ..
