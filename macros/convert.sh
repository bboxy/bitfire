#!/bin/sh
################### DASM ###################
cat ./link_macros_acme.inc | \
#comments \
#sed 's/;/;/g' | \
#remove arg
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1\t\tmac \3/g' | \
#!macro name { \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1\t\tmac \3 /g' | \
#add end \
sed 's/}/endm/g' | \
#replace arg within block \
sed 's/\(.*[[:graph:]]\)\.arg/\1\{1\}/g' | \
#remove trailing + from macros \
sed 's/\(^[[:space:]]*\)+\(.*\)/\1\2/g' \
> ./link_macros_dasm.inc
################### 64TASS ###################
cat ./link_macros_acme.inc | \
#comments \
#sed 's/;/;/g' | \
#remove arg \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1\3.macro/g' | \
#!macro name { \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1\3.macro/g' | \
#add end \
sed 's/[[:space:]]*}/.endm/g' | \
#replace arg within block \
sed 's/\(.*[[:graph:]]\)\.arg/\1\\1/g' | \
#replace .l \
sed 's/\.l/l/g' | \
#remove trailing + from macros \
sed 's/\(^[[:space:]]*\)+\(.*\)/\1\2/g' \
> ./link_macros_64tass.inc
################### CA65 ###################
cat ./link_macros_acme.inc | \
#comments \
#sed 's/;/;/g' | \
#replace .l \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1\t\t.macro \3 arg\n\t\t.local l/g' | \
#!macro name { \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1\t\t.macro \3\n\t\t.local l/g' | \
#add end \
sed 's/[[:space:]]*}/\t\t.endmacro/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' \
> ./link_macros_ca65.inc
################### KICKASS ###################
cat ./link_macros_acme.inc | \
sed 's/(/[/g' | \
sed 's/)/]/g' | \
#replace .l \
sed 's/^\.l/l:/g' | \
sed 's/^;\.l/;l:/g' | \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1.macro \3(arg) {/g' | \
#!macro name { \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1.macro \3() {/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' | \
#comments \
sed 's/;/\/\//g' \
> ./link_macros_kickass.inc
################### DREAMASS ###################
cat ./link_macros_acme.inc | \
#comments \
#replace .l \
sed 's/^\.l/l:/g' | \
sed 's/^;\.l/;l:/g' | \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1\t\t#macro \3(arg) {\n.(/g' | \
#!macro name { \
sed 's/i\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1\t\t#macro \3\n\t\t{\n.(/g' | \
#endmacro \
sed 's/[[:space:]]*}/).\n\t\t}/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' \
> ./link_macros_dreamass.inc
