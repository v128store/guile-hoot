GUILE_WARNINGS = -W2
moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES = 					\
  wasm/types.scm				\
  wasm/resolve.scm				\
  wasm/wat.scm					\
  wasm/assemble.scm				\
  wasm/disassemble.scm				\
  wasm/dump.scm					\
  wasm/link.scm					\
  wasm/parse.scm				\
  hoot/compile.scm				\
  scripts/compile-wasm.scm

SUFFIXES = .scm .go
GOBJECTS = $(SOURCES:%.scm=%.go)
CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES)
nobase_mod_DATA = $(SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
