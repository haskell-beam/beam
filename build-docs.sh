#!/usr/bin/env bash

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

usage () {
    echo "build-docs: Build beam docs"
    echo "Usage: build-docs <command>"
    echo
    echo "Where <command> is one of"
    echo "    servedocs - serve documents on 8000"
    echo "    ghpages   - build ghpages branch"
}

download_chinook_db() {
    git submodule update --init --recursive
}

prepare_sqlite () {
    if [ ! -f beam-sqlite/examples/chinook.db ]; then
        download_chinook_db
        printf "Updating SQLite chinook db... "
        sqlite3 beam-sqlite/examples/chinook.db.tmp < docs/chinook/ChinookDatabase/DataSources/Chinook_Sqlite.sql
        printf "DONE\n"
        mv beam-sqlite/examples/chinook.db.tmp beam-sqlite/examples/chinook.db
    fi
}

prepare_postgres () {
    if psql -lAt | awk -F "|" '{print $1}' | grep "^Chinook$" > /dev/null; then
        echo "Postgres Chinook database exists"
    else
        echo "Creating postgres database"
    fi
}

prepare_databases () {
    prepare_sqlite
    prepare_postgres
}

prepare_databases

case $1 in
    servedocs) servedocs ;;
    ghpages) ghpages ;;
    *) usage ;;
esac

