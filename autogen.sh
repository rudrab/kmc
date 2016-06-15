#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="KKR_DREAM"

(test -f $srcdir/configure.ac) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level directory"
    exit 1
}

(test -f $srcdir/build-aux) || {
     autoreconf --install 
}
(test -f $srcdir/configure) || {
     autoreconf --install 
}

