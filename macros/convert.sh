#!/bin/sh
################### DASM ###################
cat ./link_macros_acme.inc | \
#comments \
#sed 's/;/;/g' | \
sed 's/\r//g' | \
#remove arg
sed 's/^\([[:space:]]*\)!macro[[:space:]]*\([[:graph:]]*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1mac \2/g' | \
#!macro name { \
sed 's/^\([[:space:]]*\)!macro[[:space:]]*\([[:graph:]]*\)[[:space:]]*.*{/\1mac \2/g' | \
#add end \
sed 's/^\([[:space:]]*\)*}/\1endm/g' | \
#replace arg within block \
sed 's/\(.*[[:graph:]]\)\.arg/\1\{1\}/g' | \
#remove trailing + from macros \
sed 's/\(^[[:space:]]*\)+\(.*\)/\1\2/g' \
> ./link_macros_dasm.inc
################### 64TASS ###################
cat ./link_macros_acme.inc | \
#comments \
#sed 's/;/;/g' | \
sed 's/\r//g' | \
#remove arg \
sed 's/^\([[:space:]]*\)!macro[[:space:]]*\([[:graph:]]*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1\2 .macro/g' | \
#!macro name { \
sed 's/^\([[:space:]]*\)!macro[[:space:]]*\([[:graph:]]*\)[[:space:]]*.*{/\1\2 .macro/g' | \
#add end \
sed 's/^\([[:space:]]*\)*}/\1.endm/g' | \
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
sed 's/\r//g' | \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/^\([[:space:]]*\)!macro[[:space:]]*\([[:graph:]]*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1.macro \2 arg\n\t\t.local l/g' | \
#!macro name { \
#sed 's/\(.*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1\t\t.macro \3\n\t\t.local l/g' | \
sed 's/^\([[:space:]]*\)!macro[[:space:]]*\([[:graph:]]*\)[[:space:]]*.*{/\1.macro \2\n\t\t.local l/g' | \
#add end \
sed 's/^\([[:space:]]*\)}/\1.endmacro/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' \
> ./link_macros_ca65.inc
################### KICKASS ###################
cat ./link_macros_acme.inc | \
sed 's/\r//g' | \
sed 's/(/[/g' | \
sed 's/)/]/g' | \
#replace .l \
sed 's/^\.l/l:/g' | \
sed 's/^;\.l/;l:/g' | \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/^\([[:space:]]*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1.macro \3(arg) {/g' | \
#!macro name { \
sed 's/^\([[:space:]]*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1.macro \3() {/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' | \
#comments \
sed 's/;/\/\//g' | \
#expand macros \
sed 's/^\([[:space:]]*\)+\([[:graph:]]*\)/\1:\2()/' \
> ./link_macros_kickass.inc
################### DREAMASS ###################
cat ./link_macros_acme.inc | \
#comments \
sed 's/\r//g' | \
#replace .l \
sed 's/^\.l/l:/g' | \
sed 's/^;\.l/;l:/g' | \
sed 's/\.l/l/g' | \
#remove arg \
sed 's/^\([[:space:]]*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*\(\.arg\)[[:space:]]*{/\1\t\t#macro \3(arg) {\n.(/g' | \
#!macro name { \
sed 's/^\([[:space:]]*\)\(!macro\)[[:space:]]*\(.*\)[[:space:]]*{/\1\t\t#macro \3\n\t\t{\n.(/g' | \
#endmacro \
sed 's/^[[:space:]]*}/).\n\t\t}/g' | \
#replace arg within block \
sed 's/\.arg/arg/g' \
> ./link_macros_dreamass.inc
