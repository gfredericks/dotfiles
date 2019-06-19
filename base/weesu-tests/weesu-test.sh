#!/usr/bin/env bash

# Assumes the version of weesu meant to be tested is on the PATH

set -eouC pipefail

cd $(dirname "$0")

THE_TMP_DIR=$(mktemp -d weesu-tests-XXXXXXXXX)
trap "{ rm -rf $THE_TMP_DIR; }" EXIT
_mktemp(){
    mktemp ${1:-} $THE_TMP_DIR/XXXXXXXXXXXXX.json
}

TMP_FILE=$(_mktemp)
CATALOG1=$(_mktemp)
CATALOG2=$(_mktemp)
weesu catalog anything test-files >| $CATALOG1
weesu catalog anything\ else test-files-2 >| $CATALOG2

# Basic contents done correctly, in sorted order
echo '4c80e675b25b13866bf5da0999d8f60085ac93784cac620400ecfa5b010c54fd
6f42f5961cb3ba45587ed17941f26732c1c806794bed0060256bb545a449a715
b17171dbad37caf7d3bc590ef145ceb7475457a6faaf0f4523596ab922cdc56e
b76ae83c50d6104039c80d312402af3027661e07066325526ad997daf6362bbc
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855' \
    | diff - <(< $CATALOG1 jq -r .sha256)

# tarball paths are done correctly
echo "more-tarballs.tar.gz//test-files-2/a-direcotry/tamborine.tgz//test-files/file1.txt" \
     | diff - <(< $CATALOG1 jq -rs '.[2].seen[2].path')

# sizes are correct, and are integers
echo $'278\n608\n12\n51\n0' \
     | diff - <(< $CATALOG1 jq '.size')

# hidden files aren't included
! grep hidden-file $CATALOG1 || exit 1

CATALOG3=$(_mktemp)
weesu merge $CATALOG1 $CATALOG2 >| $CATALOG3

# merging produces seen entries for both sources
< $CATALOG3 grep b17171dbad37 \
    | jq -r '.seen[].source_id' \
    | sort \
    | uniq \
    | diff - <(echo -e "anything\nanything else")

# merging is idempotent
diff $CATALOG3 <(weesu merge $CATALOG1 $CATALOG2)
diff $CATALOG3 <(weesu merge $CATALOG1 $CATALOG3)
diff $CATALOG3 <(weesu merge $CATALOG2 $CATALOG1)
diff $CATALOG3 <(weesu merge $CATALOG2 $CATALOG3)
diff $CATALOG3 <(weesu merge $CATALOG3 $CATALOG1)
diff $CATALOG3 <(weesu merge $CATALOG3 $CATALOG2)
diff $CATALOG3 <(weesu merge $CATALOG3 $CATALOG3)

weesu stale < $CATALOG3
weesu stale < $CATALOG3 \
    | wc -l \
    | diff <(echo 0) -

# stale when a file disappears
(
    A_DIR=$(_mktemp -du)
    CATALOG1=$(_mktemp -u)
    CATALOG2=$(_mktemp -u)
    cp -r test-files-2 $A_DIR
    weesu catalog thesource $A_DIR > $CATALOG1
    rm $A_DIR/snarf
    sleep 0.1 # make sure timestamp is later
    weesu catalog thesource $A_DIR > $CATALOG2
    LINE_COUNT="$(weesu merge $CATALOG1 $CATALOG2 \
                      | weesu stale \
                      | tee $TMP_FILE \
                      | wc -l)"
    [[ "$LINE_COUNT" -eq 1 ]] || exit 1
    grep -Pq 'STALE BLOB \(bce919ddb1\) LAST SEEN IN thesource AT .{24} AT snarf' $TMP_FILE
)

# like the last one, but different, including a file being renamed
(
    A_DIR=$(_mktemp -d)
    CATALOG1=$(_mktemp -u)
    CATALOG2=$(_mktemp -u)
    echo foobar > $A_DIR/foo.txt
    echo blazzy > $A_DIR/bar.txt
    weesu catalog sourceid $A_DIR > $CATALOG1
    rm $A_DIR/foo.txt
    mv $A_DIR/{bar,baz}.txt
    weesu catalog sourceid $A_DIR > $CATALOG2
    rm $TMP_FILE
    LINE_COUNT="$(weesu merge $CATALOG1 $CATALOG2 \
                      | weesu stale \
                      | tee $TMP_FILE \
                      | wc -l)"
    [[ "$LINE_COUNT" -eq 1 ]] || exit 1
    grep -Pq 'STALE BLOB \(aec070645f\) LAST SEEN IN sourceid AT .{24} AT foo.txt' \
         $TMP_FILE
)


# TODO: merging chooses the latest seen record by source_id

# symlinks shouldn't be followed
(
    weesu catalog yeezy diabolical-symlink 2>/dev/null \
        | wc -l \
        | diff <(echo 1) -
)

rm -rf $THE_TMP_DIR
echo Tests passed\!
