#! /bin/bash

beam_doc_status() {
    echo "$@" 1>&2
}

sha256() {
    if which sha256sum 2>/dev/null >/dev/null; then
        SHA256SUM="sha256sum $1 | awk '{print \$1}'"
    elif which openssl 2>/dev/null >/dev/null; then
        SHA256SUM="openssl dgst -sha256 $1 | awk '{print \$2}'"
    else
        beam_doc_status "Cannot find sha256sum or openssl"
        exit 1
    fi
    /bin/sh -c "$SHA256SUM"
}

download () {
    CACHED_FILE=$1
    URL=$2
    EXPECTED_SHA256=$3
    CONV=$4

    if [ ! -f $CACHED_FILE ]; then
        TMP_FILE="${CACHED_FILE}.tmp"
        beam_doc_status "Downloading $URL to $CACHED_FILE.tmp..."

        DIR=$(dirname $TMP_FILE)
        mkdir -p $DIR

        curl $URL > $TMP_FILE

        ACTUAL_SUM=$(sha256 $TMP_FILE)
        if [ "$ACTUAL_SUM" != "$EXPECTED_SHA256" ]; then
            beam_doc_status "Sum mismatch, got $ACTUAL_SUM, expected $EXPECTED_SHA256"
            exit 1
        else
            if [ -z $CONV ]; then
	        cat $TMP_FILE | sed -e 's/\r$//' > $CACHED_FILE
	    else
                cat $TMP_FILE | bash -c "$CONV" | sed -e 's/\r$//' > $CACHED_FILE
	    fi
            rm $TMP_FILE
        fi

        beam_doc_status "Finished downloading"
    fi
}

if ! which pv >/dev/null 2>/dev/null; then
    beam_doc_status "No 'pv' command found, no progress indication available"
    pv () {
        beam_doc_status "Starting upload of $@"
        cat "$@"
    }
fi
