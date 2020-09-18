{ stdenv, buildPythonPackage, markdown, pep562, fetchPypi }:

buildPythonPackage rec {
  pname = "pymdown-extensions";
  version = "6.2.1";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0ywg2szsn5pg9rac4h5vly52crqy43mf0i7yl09hv2jz4x461giv";
  };

  propagatedBuildInputs = [ markdown pep562 ];

  doCheck = false;

  meta = {
    homepage = https://github.com/facelessuser/pymdown-extensions;
    description = "Extension pack for Python Markdown.";
    license = stdenv.lib.licenses.mit;
  };
}
