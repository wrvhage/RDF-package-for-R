AC_INIT(src/RDF.c)

AC_CHECK_LIB(raptor2, raptor_new_parser, [HAVE_RAPTOR2=TRUE], [HAVE_RAPTOR2=FALSE])
AC_SUBST(HAVE_RAPTOR2)

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`


: ${PKGCONFIG=pkg-config}
if ${PKGCONFIG} --version >/dev/null 2>&1; then
  if ${HAVE_RAPTOR2}; then
    PKG_CPPFLAGS=`${PKGCONFIG} --cflags raptor2`
    PKG_LIBS=`${PKGCONFIG} --libs raptor2`
  fi
fi

AC_SUBST([R_HOME],[${R_HOME}])
AC_SUBST([CPPFLAGS],[${CPPFLAGS}])
AC_SUBST([PKG_CPPFLAGS],[${PKG_CPPFLAGS}])
AC_SUBST([PKG_LIBS],[${PKG_LIBS}])

AC_OUTPUT([src/Makevars])
