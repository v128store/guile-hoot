GUILE_WARNINGS = -W2
moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES = 					\
  language/cps/hoot.scm				\
  language/cps/hoot/tailify.scm			\
  language/cps/hoot/lower-primcalls.scm		\
  language/cps/hoot/unify-returns.scm		\
  wasm/types.scm				\
  wasm/resolve.scm				\
  wasm/wat.scm					\
  wasm/assemble.scm				\
  wasm/dump.scm					\
  wasm/link.scm					\
  wasm/lower.scm				\
  wasm/lower-globals.scm			\
  wasm/lower-stringrefs.scm			\
  wasm/parse.scm				\
  wasm/canonical-types.scm			\
  wasm/stack.scm				\
  wasm/symbolify.scm				\
  wasm/vm.scm					\
  hoot/config.scm				\
  hoot/compile.scm				\
  hoot/inline-wasm.scm				\
  hoot/primitives.scm				\
  hoot/stdlib.scm				\
  hoot/reflect.scm				\
  hoot/repl.scm					\
  scripts/assemble-wasm.scm			\
  scripts/compile-wasm.scm

NOCOMP_SOURCES = 				\
  hoot/prelude.scm				\
  hoot/generate-char-prelude.scm

GENERATED_NOCOMP_SOURCES = 			\
  hoot/char-prelude.scm

hoot/char-prelude.scm: hoot/generate-char-prelude.scm
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE) $< > "$@"

SUFFIXES = .scm .go
GOBJECTS = $(SOURCES:%.scm=%.go)
CLEANFILES = $(GOBJECTS) $(GENERATED_NOCOMP_SOURCES)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GENERATED_NOCOMP_SOURCES)
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
