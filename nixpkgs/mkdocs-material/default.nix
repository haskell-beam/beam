{ stdenv, buildPythonPackage, fetchPypi, pygments, mkdocs, pymdown-extensions, mkdocs-minify-plugin }:

buildPythonPackage rec {
    pname = "mkdocs-material";
    version = "4.5.1";

    src = fetchPypi {
      inherit pname version;
      sha256 = "1cb625900imjmvzp2rs2qgafjrjdyrakzzs5lvh65smj6v98j7g1";
    };

    propagatedBuildInputs = [ pygments mkdocs pymdown-extensions mkdocs-minify-plugin ];

    meta = {
      homepage = https://squidfunk.github.io/mkdocs-material/;
      description = "A Material Design theme for MkDocs";
      license = stdenv.lib.licenses.mit;
    };
}
