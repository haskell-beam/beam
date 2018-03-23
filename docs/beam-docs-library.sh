status() {
    echo "$@" 1>&2
}
export -f status

sha256() {
    if which sha256sum 2>/dev/null >/dev/null; then
        SHA256SUM="sha256sum $1 | awk '{print \$1}'"
    elif which openssl 2>/dev/null >/dev/null; then
        SHA256SUM="openssl dgst -sha256 $1 | awk '{print \$2}'"
    else
        status "Cannot find sha256sum or openssl"
        exit 1
    fi
    /bin/sh -c "$SHA256SUM"
}
export -f sha256

download () {
    CACHED_FILE=$1
    URL=$2
    EXPECTED_SHA256=$3
    CONV=$4

    if [ ! -f $CACHED_FILE ]; then
        TMP_FILE="${CACHED_FILE}.tmp"
        status "Downloading $URL to $CACHED_FILE.tmp..."

        DIR=$(dirname $TMP_FILE)
        mkdir -p $DIR

        if [ -z $CONV ]; then
	    curl $URL | sed -e 's/\r$//' > $TMP_FILE
	else
	    curl $URL | $CONV | sed -e 's/\r$//' > $TMP_FILE
	fi

        ACTUAL_SUM=$(sha256 $TMP_FILE)
        if [ "$ACTUAL_SUM" != "$EXPECTED_SHA256" ]; then
            status "Sum mismatch, got $ACTUAL_SUM, expected $EXPECTED_SHA256"
            exit 1
        else
            mv $TMP_FILE $CACHED_FILE
        fi

        status "Finished downloading"
    fi
}
export -f download

if ! which pv >/dev/null 2>/dev/null; then
    status "No 'pv' command found, no progress indication available"
    pv () {
        status "Starting upload of $@"
        cat "$@"
    }
    export -f pv
fi
