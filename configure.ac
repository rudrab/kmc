#AC_PREREQ(2.69)
AC_INIT([src], [0.4],
	[],
	[kmc],
	[])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.9.6 dist-bzip2 subdir-objects foreign])

#AC_SEARCH_LIBS([dsyev_], [lapack], ,AC_MSG_ERROR([No LAPACK library]))
AC_OPENMP
FCFLAGS="$OPENMP_FFLAGS -fcheck=all"
AC_PROG_FC([gfortran])
AC_CHECK_PROG([DEPF90_CHECK],[makedepf90],[yes],[no])
AM_CONDITIONAL([FOUND_MAKEDEPF90], [test "x$DEPF90_CHECK" = xyes])
AC_CONFIG_FILES([
		 Makefile
		 ])
AC_OUTPUT
