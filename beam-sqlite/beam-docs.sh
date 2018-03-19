#!/bin/sh

set -e

SQLITE_DB=$1
CHINOOK_SQLITE_URL="https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sql"
EXPECTED_SHA256="a317fb95dc73c0402788727f10684d62a5331afa2d2918e24ab81233c35290f8"

print_open_statement() {
    echo "chinook <- open \"chinook.db\""
}

if [ -f $SQLITE_DB ]; then
    print_open_statement
    exit 0
fi

if [ ! -f chinook-data/Chinook_Sqlite.sql ]; then
    status "Downloading Sqlite chinook data..."
    download "chinook-data/Chinook_Sqlite.sql.tmp" "$CHINOOK_SQLITE_URL" "$EXPECTED_SHA256"

    status "Converting data"
    cat chinook-data/Chinook_Sqlite.sql.tmp | tail -c +4 > chinook-data/Chinook_Sqlite.sql.conv

    status "Finished conversion"
    mv chinook-data/Chinook_Sqlite.sql.conv chinook-data/Chinook_Sqlite.sql
    rm chinook-data/Chinook_Sqlite.sql.tmp
fi

status "Creating temporary $SQLITE_DB..."

(echo "BEGIN;"; pv chinook-data/Chinook_Sqlite.sql; echo "END;") | sqlite3 $SQLITE_DB.tmp

status "Success, creating $SQLITE_DB"

mv $SQLITE_DB.tmp $SQLITE_DB

print_open_statement
