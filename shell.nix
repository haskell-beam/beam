{ nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.haskell.packages.ghc861.ghc }:
with (import <nixpkgs> {});

let
  livereload = python27Packages.buildPythonPackage {
    name = "livereload-2.5.1";
    src = fetchurl {
      url = https://pypi.python.org/packages/e9/2e/c4972828cf526a2e5f5571d647fb2740df68f17e8084a9a1092f4d209f4c/livereload-2.5.1.tar.gz;
      sha256 = "0b2yyfnpddmrwjfqsndidzchkf3l9jlgzfkwl8dplim9gq6y2ba2";
    };

    propagatedBuildInputs = with python27Packages; [ six tornado ];

    meta = {
      homepage = https://github.com/lepture/python-livereload;
      description = "Python LiveReload is an awesome tool for web developers";
      license = stdenv.lib.licenses.bsd3;
    };
  };

  mkdocs = python27Packages.buildPythonApplication rec {
    name="mkdocs-0.17.2";
    src = fetchurl {
      url = https://pypi.python.org/packages/27/0a/bb42cda3b298ffb4b30375b7538a4d57803ff8be418ee3e00460188c4332/mkdocs-0.17.2.tar.gz;
      sha256 = "18d3m9iws5shlbg0yj5xwiy68bliiz70v32y5pa8wi274c36nssa";
    };

    propagatedBuildInputs = with python27Packages;
    [ tornado livereload click pyyaml markdown jinja2 ];

    meta = {
      homepage = http://www.mkdocs.org/;
      description = "MkDocs is a fast, simple and downright gorgeous static site generator that’s geared towards building project documentation. Documentation source files are written in Markdown, and configured with a single YAML configuration file.";
      license = stdenv.lib.licenses.bsd3;
    };
  };

  pymdown-extensions = python27Packages.buildPythonPackage {
    name = "pymdown-extensions-4.8";
    src = fetchurl {
      url = https://pypi.python.org/packages/f5/9f/74d8a85458e831f3b161956b30bc60d31c6a507ed72ac4f4cb2ca08d8042/pymdown-extensions-4.8.tar.gz;
      sha256 = "1zvi8d44v758vbhi9fl5x5gqs098ajamilfz53jzid0v0fad88nj";
    };

    propagatedBuildInputs = with python27Packages; [ markdown ];
    doCheck = false;

    meta = {
      homepage = https://github.com/facelessuser/pymdown-extensions;
      description = "Extension pack for Python Markdown.";
      license = stdenv.lib.licenses.mit;
    };
  };

  mkdocs-material = python27Packages.buildPythonPackage {
    name = "mkdocs-material-2.6.0";
    src = fetchurl {
      url = https://pypi.python.org/packages/e3/85/f42493d453d9b6f51912b818134a4a555c597807ba96b40eae12017ede35/mkdocs-material-2.6.0.tar.gz;
      sha256 = "1xq5nkj0g6gg4lm8nhcwc30g9drq1i4p4pky8s5c0rfa1s9s7sla";
    };

    propagatedBuildInputs = with python27Packages; [ pymdown-extensions pygments mkdocs ];

    meta = {
      homepage = https://squidfunk.github.io/mkdocs-material/;
      description = "A Material Design theme for MkDocs";
      license = stdenv.lib.licenses.mit;
    };
  };

  markdown-fenced-code-tabs = python27Packages.buildPythonPackage {
    name = "markdown-fenced-code-tabs-0.2.0";
    src = fetchurl {
      url = https://pypi.python.org/packages/21/7a/0cee39060c5173cbd80930b720fb18f5cb788477c03214ccdef44ec91d85/markdown-fenced-code-tabs-0.2.0.tar.gz;
      sha256 = "05k5v9wlxgghw2k18savznxc1xgg60gqz60mka4gnp8nsxpz99zs";
    };

    propagatedBuildInputs = with python27Packages; [ markdown ];

    meta = {
      homepage = https://github.com/yacir/markdown-fenced-code-tabs;
      description = "Generates Bootstrap HTML Tabs for Consecutive Fenced Code Blocks";
      license = stdenv.lib.licenses.mit;
    };
  };
in
  haskell.lib.buildStackProject {
    inherit ghc;
    name = "beam-env";
    buildInputs = [ postgresql # python27Packages.ghp-import 
                    bash
                    mkdocs
                    (python27.withPackages (ps: [ mkdocs mkdocs-material ps.sqlparse ]))
                    ncurses libcxx icu gcc mysql zlib openssl stack gnupg dos2unix vim pcre ];
    LANG = "en_us.UTF-8";
  }
