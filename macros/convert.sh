#!/bin/sh
################### DASM ###################
cat ./link_macros_acme.inc | \
#comments \
#sed 's/;/;/g' | \
#remove arg
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\t\tmac \2/g' | \
#!macro name { \
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\t\tmac \2 /g' | \
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
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\2.macro/g' | \
#!macro name { \
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\2.macro/g' | \
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
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\t\t.macro \2 arg\n\t\t.local l/g' | \
#!macro name { \
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\t\t.macro \2\n\t\t.local l/g' | \
#add end \
sed 's/[[:space:]]*}/\t\t.endmacro/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' \
> ./link_macros_ca65.inc
################### KICKASS ###################
cat ./link_macros_acme.inc | \
#comments \
sed 's/;/\/\//g' | \
sed 's/(/[/g' | \
sed 's/)/]/g' | \
#replace .l \
sed 's/^\.l/l:/g' | \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/.macro \2(arg) {/g' | \
#!macro name { \
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/.macro \2() {/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' \
> ./link_macros_kickass.inc
################### DREAMASS ###################
cat ./link_macros_acme.inc | \
#comments \
#replace .l \
sed 's/^\.l/l:/g' | \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\t\t#macro \2(arg) {\n.(/g' | \
#!macro name { \
sed 's/.*\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\t\t#macro \2\n\t\t{\n.(/g' | \
#endmacro \
sed 's/[[:space:]]*}/).\n\t\t}/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' \
> ./link_macros_dreamass.inc
