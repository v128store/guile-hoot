dnl -*- Autoconf -*-

AC_INIT(guile-hoot, 0.2.0)
AC_CONFIG_SRCDIR(module)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 foreign silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile module/Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([js-test], [chmod +x js-test])
AC_CONFIG_FILES([module/hoot/config.scm])

# Prepare a version of $datadir that does not contain references to
# shell variables.  Snarfed from Guix.
hoot_prefix="`eval echo $prefix | sed -e"s|NONE|/usr/local|g"`"
hoot_datadir="`eval eval echo $datadir | sed -e "s|NONE|$hoot_prefix|g"`"
AC_SUBST([hoot_datadir])

GUILE_PKG([3.0])
GUILE_PROGS

AC_PATH_PROG([D8], [d8])
AC_SUBST([D8])

AC_OUTPUT
