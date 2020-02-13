#!/bin/sh 

# Make a template for tm.h file from tm.texi.  This script is simple
# and does a sloppy job, but you may find it useful.
#
# <sizif@botik.ru>

awk '
BEGIN { 
        bar = "*************************************************************"; }
/^@section/ { 
        printf("\n\n/%s\n %s\n%s/\n\n", bar, substr($0,index($0," ")+1), bar);
        continue; }
/^@subsection/ { 
        name = substr($0,index($0," ")+1);
        printf("\n/*** %s ", name, 53-length(name), bar);
        for(i=53-length(name); i>0; i--)
            printf("*");
        printf("*/\n\n");
        continue; }
/^@item[a-z]* [A-Z]/ {
        printf("/* #define %s */\n", substr($0,index($0," ")+1));
        continue; }
/^@item[a-z]*/ && length($2) != 0 {
        printf("/* %s */\n", substr($0,index($0," ")+1)); }

' | sed -e 's,@var{\([^}]*\)},\1,g' -e 's, (,(,g'
