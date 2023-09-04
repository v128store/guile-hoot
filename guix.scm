(use-modules (guix)
             (guix build-system cmake)
             (guix build-system gnu)
             (guix gexp)
             (guix git)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages gawk)
             (gnu packages glib)
             (gnu packages guile)
             (gnu packages icu4c)
             (gnu packages llvm)
             (gnu packages ninja)
             (gnu packages pkg-config)
             (gnu packages perl)
             (gnu packages python)
             (gnu packages version-control))

(define guile-next-next
  (let ((commit "f31819b6b179429a617c8bd881dbb61219823e39")
        (revision "1"))
    (package
     (inherit guile-next)
     (version (git-version "3.0.9" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/guile.git")
             (commit commit)))
       (file-name (git-file-name "guile" version))
       (sha256
        (base32 "1pnhihi8bn863vjj4xip3d0gpg3k5kkc1vs56af5fakhai9pjfsh")))))))

(define gn
  (let ((commit "1de45d1a11cc9f8cb5c75a031386151e1c384847")
        (revision "1"))
    (package
      (name "gn")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gn.googlesource.com/gn.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12bcpmgxyb3i6qvr7ipw9dfhvbj5dllg8r2gm9ahkgh4zlf40jfk"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'configure
                   (lambda _
                     ;; The build system inspects the git history and
                     ;; parses the output of "git describe" and
                     ;; assumes that the current state of the repo is
                     ;; at least one commit past the "initial-commit"
                     ;; tag.  Oof!
                     (invoke "git" "init")
                     (invoke "git" "config" "user.name" "Guix")
                     (invoke "git" "config" "user.email" "guix@guix.gnu.org")
                     (invoke "git" "add" "README.md")
                     (invoke "git" "commit" "-m" "Initial commit.")
                     (invoke "git" "tag" "--annotate" "--message=Initial commit."
                             "initial-commit")
                     (invoke "git" "add" "LICENSE")
                     (invoke "git" "commit" "-m" "Second commit.")
                     (invoke "./build/gen.py")))
                 (replace 'build
                   (lambda _
                     (invoke "ninja" "-C" "out")))
                 (replace 'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin")))
                       (mkdir-p bin)
                       (install-file "out/gn" bin)))))))
      (native-inputs (list clang git ninja python))
      (home-page "https://gn.googlesource.com/gn/")
      (synopsis "Google's Ninja wrapper")
      (description "GN is Google's wrapper around the Ninja build system.")
      (license license:bsd-3))))

(define-syntax-rule (define-chromium-origin name path commit-sha hash)
  (define name
    (let ((commit commit-sha))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url (string-append "https://chromium.googlesource.com/"
                                  path
                                  ".git"))
              (commit commit)))
        (file-name (git-file-name (symbol->string 'name) "0"))
        (sha256
         (base32 hash))))))

;; Based on the dependencies listed in the DEPS file in the v8 repo.
(define-chromium-origin depot-tools
  "chromium/tools/depot_tools"
  "e5277508c83c3dd1596504daf64b6ea8fe7d2dd1"
  "18bbb9dgjxvmab9v7rl9cnh4prig69n3c68kb4c1a332ll5m7k7s")

(define-chromium-origin buildtools
  "chromium/src/buildtools"
  "1cc82962cb50a35f6008b25a165782c568edac27"
  "1lvacf9rfqjwx8h5dqz0w8y1vl4d66ws5v887a93dhrlhp7li1s6")

(define-chromium-origin chromium-build
  "chromium/src/build"
  "143d726c8ef21c30bc50ec4436a4d10aca156f4c"
  "17a255dpgxm3b0qgjkbylgbb75qsfhnp4c1ik7fw85ifr89mm2d1")

(define-chromium-origin icu
  "chromium/deps/icu"
  "e8c3bc9ea97d4423ad0515e5f1c064f486dae8b1"
  "0h6lw8gv6yfashza969y6g3m3cbg3wm7g7hqf3z65rcqdvms5xa8")

(define-chromium-origin zlib
  "chromium/src/third_party/zlib"
  "7eff33bc00be3e2ddb55a94991689fad9dcbe85d"
  "01qssvqgls1plyfpr4775fkrm0jmsjw8ywqr8cf6mi2aw804m84i")

(define-chromium-origin googletest
  "external/github.com/google/googletest"
  "af29db7ec28d6df1c7f0f745186884091e602e07"
  "0f7g4v435xh830npqnczl851fac19hhmzqmvda2qs3fxrmq6712m")

(define-chromium-origin trace-event
  "chromium/src/base/trace_event/common"
  "147f65333c38ddd1ebf554e89965c243c8ce50b3"
  "0vhm9894gjfwn5jsbcql8f6di5w3hzh7rd1j5fysg72lcwj14jas")

(define-chromium-origin jinja2
  "chromium/src/third_party/jinja2"
  "515dd10de9bf63040045902a4a310d2ba25213a0"
  "0gh8xpnbl9lq82ggxpv0q7a67pvcdmcrl7r8z3hk9awdjq4dbvvy")

(define-chromium-origin markupsafe
  "chromium/src/third_party/markupsafe"
  "006709ba3ed87660a17bd4548c45663628f5ed85"
  "1ql3sdwjwc0b19hbz4v55m81sf3capl9xc5ya9nq1l7kclj72m06")

(define-chromium-origin abseil-cpp
  "chromium/src/third_party/abseil-cpp"
  "ae5ee2a38fc234109fb54e3845e6b427667406b8"
  "1f4qmcqj9s9hz4i2kpxfnq6lg4dq2l9v5r1bbpfavsfialg0i7j0")

;; V8 uses the gold linker, so we need to wrap it in order for the
;; Guix runpath magic to work.
(define ldwrapper-gold
  (make-ld-wrapper "gold-ldwrapper"
                   #:binutils binutils-gold
                   #:linker "ld.gold"))

(define v8
  (let ((commit "b04b184405b8f1989f15844105cfacf0500817a8")
        (revision "1"))
    (package
      (name "v8")
      (version (git-version "11.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://chromium.googlesource.com/v8/v8.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zvp8kzpbsg6x7lwgc0xyx6vd5dswkjp5f0g0wlahfp5f8ia6bcn"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'patch-source-shebangs 'install-depot-tools
                   (lambda _
                     ;; Install all the necessary submodules.  The
                     ;; DEPS file specifies many more than this but
                     ;; these seem to the minimal set required for
                     ;; what we're doing.
                     (copy-recursively #$chromium-build "build")
                     (copy-recursively #$buildtools "buildtools")
                     (copy-recursively #$trace-event "base/trace_event/common")
                     (copy-recursively #$depot-tools "third_party/depot_tools")
                     (copy-recursively #$icu "third_party/icu")
                     (copy-recursively #$zlib "third_party/zlib")
                     (copy-recursively #$googletest "third_party/googletest/src")
                     (copy-recursively #$jinja2 "third_party/jinja2")
                     (copy-recursively #$markupsafe "third_party/markupsafe")
                     (copy-recursively #$abseil-cpp "third_party/abseil-cpp")
                     ;; Install Google's ninja wrapper executable.
                     (install-file (string-append #$gn "/bin/gn")
                                   "buildtools/linux64")
                     ;; Build fails if this file doesn't exist.
                     (call-with-output-file "build/config/gclient_args.gni"
                       (lambda (port) #t))
                     ;; depot_tools need to be on PATH.
                     (setenv "PATH"
                             (string-append (getcwd)
                                            "/third_party/depot_tools:"
                                            (getenv "PATH")))))
                 (replace 'configure
                   (lambda _
                     (define args
                       ;; These flags are taken from the built-in
                       ;; x64.release config:
                       '("is_component_build = false"
                         "is_debug = false"
                         "target_cpu = \"x64\""
                         "v8_enable_sandbox = true"
                         "use_goma = false"
                         "v8_enable_backtrace = true"
                         "v8_enable_disassembler = true"
                         "v8_enable_object_print = true"
                         "v8_enable_verify_heap = true"
                         "dcheck_always_on = false"
                         ;; These flags are our custom ones:
                         "is_official_build = false"
                         ;; Build with GCC.
                         "is_clang = false"
                         ;; Don't use Google's libc++.
                         "use_custom_libcxx = false"
                         ;; Don't try to download a whole Debian
                         ;; image, please.  Thanks!
                         "use_sysroot = false"
                         ;; The build fails due to some warnings when
                         ;; this flag is enabled.
                         "treat_warnings_as_errors = false"))
                     ;; There are some "fatal" messages that don't
                     ;; actually appear to be fatal if this isn't a
                     ;; git repo, but we just initialize one to be
                     ;; safe.
                     (invoke "git" "init")
                     ;; Bootstrap the build system.
                     (invoke "gn" "gen" "out/x64"
                             (string-append "--args="
                                            (string-join args " ")))))
                 (replace 'build
                   (lambda _
                     (invoke "ninja" "-C" "out/x64" "d8")))
                 (replace 'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin")))
                       (mkdir-p bin)
                       (install-file "out/x64/d8" bin)
                       (install-file "out/x64/mksnapshot" bin)
                       (install-file "out/x64/torque" bin)
                       ;; d8 won't start without this.
                       (install-file "out/x64/snapshot_blob.bin" bin))))
                 ;; TODO: Runpath stuff isn't working properly even
                 ;; though I'm using a wrapped linker!
                 (delete 'validate-runpath))))
      (native-inputs (list git ldwrapper-gold ninja perl pkg-config python))
      (inputs (list glib))
      (home-page "https://v8.dev")
      (synopsis "V8 JavaScript/WASM engine.")
      (description "You should have had a V8.")
      (license license:bsd-3))))

(package
  (name "guile-hoot")
  (version "0.1.0-git")
  (source (git-checkout (url (dirname (current-filename)))))
  (build-system gnu-build-system)
  (arguments
   '(#:make-flags '("GUILE_AUTO_COMPILE=0")))
  (native-inputs
   (list autoconf automake pkg-config))
  (inputs
   (list guile-next-next v8))
  (synopsis "WASM compiler for Guile Scheme")
  (description "Guile-hoot is an ahead-of-time WebAssembly compiler for GNU Guile.")
  (home-page "https://spritely.institute")
  (license (list license:asl2.0 license:lgpl3+)))
