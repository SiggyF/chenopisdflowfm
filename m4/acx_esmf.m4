# ===========================================================================
#          http://....somewhere.../acx_esmf.m4
# ===========================================================================
#
# SYNOPSIS
#
#   AX_ESMF([ACTION-IF-FOUND[, ACTION-IF-NOT-FOUND]])
#
# DESCRIPTION
#
#   This macro tries to find out how to compile programs that use ESMF
#   (Earth System Modelling Framework), a standard API for model coupling
#   see http://www.earthsystemmodeling.org
#

#   If either the module file (esmf) or the library (libesmf) is not found,
#   The list of shell commands in ACTION-IF-NOT-FOUND is executed. If it is
#   found the ACTION-IF-FOUND list is executed.
#   If ACTION-IF-FOUND is not specified, the default action will
#   define HAVE_ESMF.
#   The configuration exits on error, asking for a valid esmf
#   installation directory or --without-esmf.
#
#
# LICENSE
#
#   Copyright (c) 2013 Fedor Baart <fedor.baart@deltares.nl>
#
#   This program is free software: you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published by the
#   Free Software Foundation, either version 3 of the License, or (at your
#   option) any later version.
#
#   This program is distributed in the hope that it will be useful, but
#   WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
#   Public License for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with this program. If not, see <http://www.gnu.org/licenses/>.
#
#   As a special exception, the respective Autoconf Macro's copyright owner
#   gives unlimited permission to copy, distribute and modify the configure
#   scripts that are the output of Autoconf when processing the Macro. You
#   need not follow the terms of the GNU General Public License when using
#   or distributing such scripts, even though portions of the text of the
#   Macro appear in them. The GNU General Public License (GPL) does govern
#   all other use of the material that constitutes the Autoconf Macro.
#
#   This special exception to the GPL applies to versions of the Autoconf
#   Macro released by the Autoconf Archive. When you make and distribute a
#   modified version of the Autoconf Macro, you may extend this special
#   exception to the GPL to apply to your modified version as well.

AU_ALIAS([ACX_ESMF], [AX_ESMF])
AC_DEFUN([AX_ESMF], [
AC_PREREQ(2.50) dnl for AC_LANG_CASE

#
# Handle user hints
#
AC_MSG_CHECKING(if esmf is wanted)
esmf_places="/usr/local /usr /opt/local /sw"
AC_ARG_WITH([esmf],
[  --with-esmf@<:@[=DIR]@:>@       Root of esmf @<:@default=/usr/local,/usr,/opt/local,/sw @:>@ ],
[if test "$withval" != no ; then
  AC_MSG_RESULT(yes)
  if test -d "$withval"
  then
    esmf_places="$withval $esmf_places"
  else
    if test "x$withval" != xyes; then
      AC_MSG_WARN([Sorry, $withval does not exist, checking usual places])
    fi
  fi
else
  esmf_places=
  AC_MSG_RESULT(no)
fi],
[
AC_MSG_RESULT(no)
esmf_places=""
])

dnl now the value esmf_places defines if we're gonna look for esmf
if test -n "${esmf_places}"
   then
   # check the user supplied or any other more or less 'standard' place:
   #   Most UNIX systems      : /usr/local and /usr
   #   MacPorts / Fink on OSX : /opt/local respectively /sw
   for ESMF_HOME in ${esmf_places} ; do
     if test -f "${ESMF_HOME}/include/esmf.mod"; then break; fi
     ESMF_HOME=""
   done
   ESMF_OLD_LDFLAGS=$LDFLAGS
   ESMF_OLD_FCFLAGS=$FCFLAGS
   if test -n "${ESMF_HOME}"; then
      LDFLAGS="$LDFLAGS -L${ESMF_HOME}/lib"
      FCFLAGS="$FCFLAGS -I${ESMF_HOME}/include"
   fi

   AC_LANG_PUSH([Fortran])
   if test x = x"$ESMFLIBS"; then
      dnl check for a c function, we can only check the fortran functions using the module
      AC_LANG_PUSH([C])
      AC_CHECK_LIB(esmf, ESMC_Initialize, [ESMFLIBS="-lesmf"])
      AC_LANG_POP
   fi

   if test x != x"$ESMFLIBS"; then
      AC_MSG_CHECKING([for esmf])
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[      use esmf])],[AC_MSG_RESULT(yes)], [ESMFLIBS=""
      AC_MSG_RESULT(no)])
   fi
   AC_LANG_POP
   AC_SUBST(ESMFLIBS)

   # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
   if test x = x"$ESMFLIBS"; then
      #
      # If either header or library was not found, action-if-not-found
      #
      m4_default([$2],[
        AC_MSG_ERROR([either specify add the ESMF home directory using --with-esmf=$DIR, add it to the appropriate include paths (FCFLAGS=-I) or disable ESMF usage with --without-esmf])
      ])
        :
   else
      ifelse([$1],,[AC_DEFINE(HAVE_ESMF,1,[Define if you have the ESMF library.])],[$1])
        :
  fi
fi
])dnl AX_ESMF
