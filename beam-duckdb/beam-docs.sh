#!/usr/bin/env bash

set -e

. ${BEAM_DOCS_LIBRARY}

DUCKDB_DB=$1

print_open_statement() {
    echo "chinook <- open \"chinook.db\""
}

if [ -f $DUCKDB_DB ]; then
    print_open_statement
    exit 0
fi

beam_doc_status "Creating temporary $DUCKDB_DB..."

rm -f $DUCKDB_DB.tmp
duckdb $DUCKDB_DB.tmp < chinook-data/Chinook_DuckDB.sql

beam_doc_status "Success, creating $DUCKDB_DB"

mv $DUCKDB_DB.tmp $DUCKDB_DB

print_open_statement
