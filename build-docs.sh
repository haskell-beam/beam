#!/usr/bin/env bash

set -e

get_pythonpath () {
    export PYTHONPATH=$(python -c "import sys; print ':'.join(['.'] + [p for p in sys.path if p != ''])")
}

servedocs () {
    get_pythonpath
    mkdocs serve
}

ghpages () {
    get_pythonpath
    mkdocs gh-deploy
}

cleandocs () {
    if [ -f "docs/.beam-query-cache" ]; then
	rm -rf docs/.beam-query-cache
    fi
}

usage () {
    echo "build-docs: Build beam docs"
    echo "Usage: build-docs <command>"
    echo
    echo "Where <command> is one of"
    echo "    servedocs - serve documents on 8000"
    echo "    ghpages   - build ghpages branch"
}

case $1 in
    clean) cleandocs ;;
    servedocs)
	servedocs ;;
    ghpages)
	ghpages ;;
    *) usage ;;
esac

