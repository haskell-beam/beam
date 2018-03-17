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
    if [ -f "docs/ChinookData" ]; then
	rm -rf docs/ChinookData
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

download_chinook_db() {
    CHINOOK_DATA="https://codeload.github.com/lerocha/chinook-database/zip/master"
    SHA256_EXPECTED="5a5738209ce962a8796e3143768ab5aa603cdc6e94fb3a650cc65dbda94ae116"

    DEST_FILE="docs/ChinookData/.master.zip"
    FINAL_FILE="docs/ChinookData/master.zip"

    if [ ! -f $FINAL_FILE ]; then
	echo "Downloading chinook data..."
	curl $CHINOOK_DATA > $DEST_FILE

	SHA256SUM="sha256sum $DEST_FILE"
	if ! which sha256sum ; then
	    if which openssl ; then
		SHA256SUM="openssl dgst -sha256 $DEST_FILE | awk '{print \$2}'"
	    else
		echo "Cannot find sha256sum or openssl"
		exit 1
	    fi
	fi
	SHA256_ACTUAL=$(bash -c "$SHA256SUM")

	if [ "$SHA256_ACTUAL" != "$SHA256_EXPECTED" ]; then
	    echo "SHA256 sum mismatch, got ${SHA256_ACTUAL}, expected ${SHA256_EXPECTED}"
	fi

	mv $DEST_FILE $FINAL_FILE

	echo "Unzipping data..."
	pushd docs/ChinookData
	unzip master
	popd
    fi
}

prepare_sqlite () {
    if [ ! -f docs/ChinookData/chinook.db ]; then
	rm -rf docs/ChinookData/chinook.db.tmp

        download_chinook_db
        printf "Updating SQLite chinook db... "
        cat docs/ChinookData/chinook-database-master/ChinookDatabase/DataSources/Chinook_Sqlite.sql | sed -e 's/\r$//' | tail -c +4 | sqlite3 docs/ChinookData/chinook.db.tmp
        printf "DONE\n"
        mv docs/ChinookData/chinook.db.tmp docs/ChinookData/chinook.db
    fi
}

prepare_postgres () {
    if psql -lAt | awk -F "|" '{print $1}' | grep "^beam-doc-chinook$" > /dev/null; then
        echo "Postgres Chinook database exists"
    else
        echo "Creating postgres database"
	psql -d template1 -c "DROP DATABASE IF EXISTS \"beam-doc-chinook-tmp\""
	psql -d template1 -c "CREATE DATABASE \"beam-doc-chinook-tmp\""
        iconv -f ISO-8859-2 -t UTF-8 docs/ChinookData/chinook-database-master/ChinookDatabase/DataSources/Chinook_PostgreSql.sql | psql -d "beam-doc-chinook-tmp" >/dev/null
	psql -d template1 -c "ALTER DATABASE \"beam-doc-chinook-tmp\" RENAME TO \"beam-doc-chinook\""
    fi
}

prepare_databases () {
    if [ ! -d docs/ChinookData ]; then
	mkdir docs/ChinookData
    fi

    echo "Verifying databases"
    prepare_sqlite
    prepare_postgres
}

case $1 in
    clean) cleandocs ;;
    servedocs)
	prepare_databases
	servedocs ;;
    ghpages)
	prepare_databases
	ghpages ;;
    *) usage ;;
esac

