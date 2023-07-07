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

(define guile-tailify
  (let ((commit "f8eaee90519207d0090bc9f74f5e677bb4ff8a64")
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
        (base32 "0254jap87n96v6xjj1p3bbb5s898sby12k21wylfd7ll1iz93aqm")))))))

(define gn
  (let ((commit "b25a2f8c2d33f02082f0f258350f5e22c0973108")
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
          (base32 "075p4jwk1apvwmqmvhwfw5f669ci7nxwjq9mz5aa2g5lz4fkdm4c"))))
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
  "019e73a13bf58542f2572daba6fbc5e389c40607"
  "16i23b97m68gvw1hix4sm4dhxvyyyh28ycfgp1ypyy50dlc3v30v")

(define-chromium-origin buildtools
  "chromium/src/buildtools"
  "64174283d074639f036ba9496f694bce2fe98f46"
  "0yipxzdi3qiprsprp4a1w40k7j44r1a1hsd8dn5xy921fzmzbq32")

(define-chromium-origin chromium-build
  "chromium/src/build"
  "8d69f8b8423a1af5d1d85ba7b3921918b40a942a"
  "18qn58fx2racn7rg2a1w8hwpidcidd53x1bmmxakwwhysqdv1sx8")

(define-chromium-origin icu
  "chromium/deps/icu"
  "d8daa943f64cd5dd2a55e9baf2e655ab4bfa5ae9"
  "0rfv6a820nc5w68isph5hrlbpyfxylwhw03s7ffijv85j9pz3dg3")

(define-chromium-origin zlib
  "chromium/src/third_party/zlib"
  "b890619bc2b193b8fbe9c1c053f4cd19a9791d92"
  "1qg86yx20zqqn67qsf5lcwafr2jsd9l270vk7gppmf3j311pgbz9")

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
  "264c07d7e64f2874434a3b8039e101ddf1b01e7e"
  "0danznscxcqai4427370h7prpz8k9qw0nf59dr9zs9252cq18j2p")

(define-chromium-origin markupsafe
  "chromium/src/third_party/markupsafe"
  "13f4e8c9e206567eeb13bf585406ddc574005748"
  "1kpqw2ld1n7l99ya6d77d0pcqgsk3waxjasrhqpk29bvk2n5sffy")

;; V8 uses the gold linker, so we need to wrap it in order for the
;; Guix runpath magic to work.
(define ldwrapper-gold
  (make-ld-wrapper "gold-ldwrapper"
                   #:binutils binutils-gold
                   #:linker "ld.gold"))

(define v8
  (let ((commit "54924a08c59282af37576a44e556bca1ca770798")
        (revision "1"))
    (package
      (name "v8")
      (version (git-version "11.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://chromium.googlesource.com/v8/v8.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0b3asl2msvjiijsdjcibmc5aa99fjcvqca2xw2xsnrq7p7rahrlr"))))
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
   (list guile-tailify v8))
  (synopsis "WASM compiler for Guile Scheme")
  (description "Guile-hoot is an ahead-of-time WebAssembly compiler for GNU Guile.")
  (home-page "https://spritely.institute")
  (license (list license:asl2.0 license:lgpl3+)))
