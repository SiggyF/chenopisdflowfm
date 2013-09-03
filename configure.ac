#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# $Revision: 1252 $

AC_PREREQ(2.59)
AC_INIT(swanfm, [0.01], [f.baart@tudelft.nl])

AC_CONFIG_SRCDIR([swanfmapp.F90])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(m4)
AM_INIT_AUTOMAKE

AC_CONFIG_HEADERS([config.h])

AC_ARG_WITH([swan],
 [AS_HELP_STRING([--with-swan],
   [enable swan support @<:@default=check@:>@])],
   [],
   [with_swan=check])


AC_ARG_WITH([dflowfm],
 [AS_HELP_STRING([--with-dflowfm],
   [enable dflowfm support @<:@default=check@:>@])],
   [],
   [with_dflowfm=check])

# prefer compilers we tested
AC_PROG_FC([gfortran ifort])
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_LIBTOOL
# Checks for libraries.
# # Check plugins in c, because we expect C compatible libraries...
AC_LANG_PUSH([Fortran])
LIBSWAN=
AS_IF([test "x$with_swan" != xno],
dnl check for one of the fortran functions
[AC_CHECK_LIB([swanesmf], [swcomp],
   [AC_SUBST([LIBSWAN], ["-lswanesmf"])
    AC_DEFINE([HAVE_LIBSWAN], [1],
              [Define if you have libswanesfm])
   ],
   [if test "x$with_swan" != xcheck; then
      AC_MSG_FAILURE(
        [--with-swan was given, but test for swan failed])
    fi
   ], [])])
LIBDFLOWFM=
AS_IF([test "x$with_dflowfm" != xno],
dnl look for an arbitrary fortran function in dflowfm
 [AC_CHECK_LIB([dflowfm], [stopja],
   [AC_SUBST([LIBDFLOWFM], ["-ldflowfm"])
    AC_DEFINE([HAVE_LIBDFLOWFM], [1],
              [Define if you have libdflowfm])
   ],
   [if test "x$with_dflowfm" != xcheck; then
      AC_MSG_FAILURE(
        [--with-dflowfm was given, but test for dflowfm failed])
    fi
   ], [])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[      use unstruc_esmf])],[], [])

   ])
dnl Also define automake options, so we can conditionally compile the ESMF modules...
AM_CONDITIONAL([HAVE_LIBSWAN],test "x$with_swan" != "xno")
AM_CONDITIONAL([HAVE_LIBDFLOWFM],test "x$with_dflowfm" != "xno")

AC_LANG_POP



# Configure mpif90 as the default compiler and make sure the c++ library is also linked
AC_LANG_PUSH(Fortran)
ACX_MPI
dnl Check for openmp for fortran
AC_OPENMP


AC_LANG_POP
FC=$MPIFC
AC_LANG_PUSH(C++)
ACX_MPI
AC_LANG_POP

# Use very long fortran lines, otherwise I can't get the ESMFCHECK preprocess macro to work
AC_FC_LINE_LENGTH([unlimited])

AC_CHECK_LIB([stdc++], [main])

dnl not prefered, but added for compatibility
AC_PATH_PROG(PKG_CONFIG,pkg-config)
dnl Check for netcdf-fortran (the new package), if found also check for netcdf-cxx.
dnl If netcdf-fortran is not found check for the old package (netcdf).
PKG_CHECK_MODULES([NETCDF_FORTRAN], netcdf-fortran,[PKG_CHECK_MODULES([NETCDF_CXX4],netcdf-cxx4)],[PKG_CHECK_MODULES([NETCDF], netcdf >= 4)])


ACX_ESMF
# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.


AC_CONFIG_FILES([Makefile
        src/Makefile
         ])
AC_OUTPUT
