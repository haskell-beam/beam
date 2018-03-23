#!/bin/sh

set -e

SQLITE_DB=$1
CHINOOK_SQLITE_URL="https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sql"
EXPECTED_SHA256="b2e430ec8cb389509d25ec5bda2f958bbf6f0ca42e276fa5eb3de45eb816a460"

print_open_statement() {
    echo "chinook <- open \"chinook.db\""
}

if [ -f $SQLITE_DB ]; then
    print_open_statement
    exit 0
fi

if [ ! -f chinook-data/Chinook_Sqlite.sql ]; then
    status "Downloading Sqlite chinook data..."
    download "chinook-data/Chinook_Sqlite.sql" "$CHINOOK_SQLITE_URL" "$EXPECTED_SHA256" "tail -c +4"
fi

status "Creating temporary $SQLITE_DB..."

(echo "BEGIN;"; pv chinook-data/Chinook_Sqlite.sql; echo "END;") | sqlite3 $SQLITE_DB.tmp

status "Success, creating $SQLITE_DB"

mv $SQLITE_DB.tmp $SQLITE_DB

print_open_statement
