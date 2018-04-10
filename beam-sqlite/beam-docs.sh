#!/usr/bin/env bash

set -e

. ${BEAM_DOCS_LIBRARY}

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
    beam_doc_status "Downloading Sqlite chinook data..."
    download "chinook-data/Chinook_Sqlite.sql" "$CHINOOK_SQLITE_URL" "$EXPECTED_SHA256" "tail -c +4"
fi

beam_doc_status "Creating temporary $SQLITE_DB..."

rm -f $SQLITE_DB.tmp
(echo "BEGIN;"; pv chinook-data/Chinook_Sqlite.sql; echo "END;") | sqlite3 $SQLITE_DB.tmp

beam_doc_status "Success, creating $SQLITE_DB"
sqlite3 $SQLITE_DB.tmp <<EOF
ALTER TABLE [Invoice] RENAME TO [InvoiceTemp];
CREATE TABLE [InvoiceNew]
(
    [InvoiceId] INTEGER NOT NULL,
    [CustomerId] INTEGER  NOT NULL,
    [InvoiceDate] DATETIME  NOT NULL,
    [BillingAddress] NVARCHAR(70),
    [BillingCity] NVARCHAR(40),
    [BillingState] NVARCHAR(40),
    [BillingCountry] NVARCHAR(40),
    [BillingPostalCode] NVARCHAR(10),
    [Total] NUMERIC(10,2)  NOT NULL
);
INSERT INTO [InvoiceNew] SELECT * FROM [InvoiceTemp];
DROP TABLE [InvoiceTemp];
CREATE TABLE [Invoice]
(
    [InvoiceId] INTEGER PRIMARY KEY AUTOINCREMENT,
    [CustomerId] INTEGER  NOT NULL,
    [InvoiceDate] DATETIME  NOT NULL,
    [BillingAddress] NVARCHAR(70),
    [BillingCity] NVARCHAR(40),
    [BillingState] NVARCHAR(40),
    [BillingCountry] NVARCHAR(40),
    [BillingPostalCode] NVARCHAR(10),
    [Total] NUMERIC(10,2)  NOT NULL,
    FOREIGN KEY ([CustomerId]) REFERENCES [Customer] ([CustomerId])
		ON DELETE NO ACTION ON UPDATE NO ACTION
);
INSERT INTO [Invoice] SELECT * FROM [InvoiceNew];
DROP TABLE [InvoiceNew];
EOF

mv $SQLITE_DB.tmp $SQLITE_DB

print_open_statement
