{ stdenv, buildPythonPackage, fetchPypi, htmlmin, jsmin, mkdocs }:

buildPythonPackage rec {
  pname = "mkdocs-minify-plugin";
  version = "0.3.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1vn40rcl76h8zznz0zrx807hjsmlsk1r8bgfphq0zfcwvf6wvzh6";
  };

  propagatedBuildInputs = [ htmlmin jsmin mkdocs ];

  meta = {
    homepage = https://github.com/byrnereese/mkdocs-minify-plugin;
    description = "An MkDocs plugin to minify HTML and/or JS files prior to being written to disk.";
    license = stdenv.lib.licenses.mit;
  };
}
