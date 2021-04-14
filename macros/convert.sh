#!/bin/bash
#!macro name .arg {
cat ./link_macros_acme.inc | \
#comments
sed 's/;/;/g' | \
#remove arg
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\t\tmac \2 /g' | \
#!macro name {
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\t\tmac \2 /g' | \
#add end
sed 's/}/endm/g' | \
#replace arg within block
sed 's/\(.*[[:graph:]]\)\.arg/\1\{1\}/g' | \
#remove trailing + from macros
sed 's/\([[:space:]]*\)+\(.*\)/\1\2/g'
