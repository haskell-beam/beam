#!/usr/bin/env bash

set -e

get_pythonpath () {
    export PYTHONPATH="`python -c "import sys; print(':'.join([s for s in sys.path if len(s) > 0]))"`:."
}

servedocs () {
    get_pythonpath
    mkdocs serve
}

ghpages () {
    get_pythonpath
    mkdocs gh-deploy
}

builddocs () {
    get_pythonpath
    mkdocs build
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
    echo "    builddocs - build docs in site/ directory"
    echo "    ghpages   - build ghpages branch"
    echo "    servedocs - serve documents on 8000"
}

case $1 in
    clean) cleandocs ;;
    servedocs)
	servedocs ;;
    builddocs)
        builddocs ;;
    ghpages)
	ghpages ;;
    *) usage ;;
esac

