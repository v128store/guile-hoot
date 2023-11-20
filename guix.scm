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
             (gnu packages guile-xyz)
             (gnu packages icu4c)
             (gnu packages llvm)
             (gnu packages ninja)
             (gnu packages pkg-config)
             (gnu packages perl)
             (gnu packages python)
             (gnu packages texinfo)
             (gnu packages version-control))

(define guile-next-next
  (let ((commit "5959531c54d1a164e638731b8d79633f454a3dbd")
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
        (base32 "07a1mlf6pxdlnn7sv2a4y5s1j6ap84h3lh6xi1y4pj8j5d5zrwdy")))))))

(define gn
  (let ((commit "991530ce394efb58fcd848195469022fa17ae126")
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
          (base32 "1zpbaspb2mncbsabps8n1iwzc67nhr79ndc9dnqxx1w1qfvaldg2"))))
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
  "4c1d6d90bc4326377ce670b74735029db9acde6a"
  "1ydr05wdc52brzvpmj5gyyxf18fqiw7yfw691ia1c49b48a77y1p")

(define-chromium-origin buildtools
  "chromium/src/buildtools"
  "ece39eb658cb3e839b5b63d5e311d72252047969"
  "190kb66r1pcl7m2ha89irv06ls17y3fw0jfz3bdgfrvbbsrgx71m")

(define-chromium-origin chromium-build
  "chromium/src/build"
  "1f9a9ca7591f832a64ca2433efaf3d6f4ea4c0ec"
  "0950v5an04c223ld4l5pfw5wlrmmwcdgghr6nnq9acyb1g7585vh")

(define-chromium-origin icu
  "chromium/deps/icu"
  "985b9a6f70e13f3db741fed121e4dcc3046ad494"
  "13rzzajqc4q1w4v9w7jx8bsmcbklngv7sa2lk2vcf22lvigkpnp9")

(define-chromium-origin zlib
  "chromium/src/third_party/zlib"
  "3f0af7f1d5ca6bb9d247f40b861346627c3032a1"
  "0m4spq3670mkjsm9yl3ysz63zd1gqjd2dyzxd3l35nb03y1pr2jk")

(define-chromium-origin googletest
  "external/github.com/google/googletest"
  "af29db7ec28d6df1c7f0f745186884091e602e07"
  "0f7g4v435xh830npqnczl851fac19hhmzqmvda2qs3fxrmq6712m")

(define-chromium-origin trace-event
  "chromium/src/base/trace_event/common"
  "29ac73db520575590c3aceb0a6f1f58dda8934f6"
  "1c25i8gyz3z36gp192g3cshaj6rd6yxi6m7j8mhw7spaarprzq12")

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
  "7207ed23d56aa19796ffd08b8203f7af7f3b5f29"
  "1db5mh7j0im0wkn3hppq8zipr35lyx97h88xcj1n9kfkm1rwhd2l")

;; V8 uses the gold linker, so we need to wrap it in order for the
;; Guix runpath magic to work.
(define ldwrapper-gold
  (make-ld-wrapper "gold-ldwrapper"
                   #:binutils binutils-gold
                   #:linker "ld.gold"))

(define v8
  (let ((commit "155ea5ca7e6e04c6126cbc6e52183c3f24ba697e")
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
          (base32 "1sw8sb48cajamkzn86q0i4r0dvv10pxn15s4r79q0jsmzq7z4sag"))))
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
   (list autoconf automake guile-syntax-highlight pkg-config texinfo))
  (inputs
   (list guile-next-next v8))
  (synopsis "WASM compiler for Guile Scheme")
  (description "Guile-hoot is an ahead-of-time WebAssembly compiler for GNU Guile.")
  (home-page "https://spritely.institute")
  (license (list license:asl2.0 license:lgpl3+)))
