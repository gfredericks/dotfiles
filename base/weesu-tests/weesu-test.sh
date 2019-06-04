#!/usr/bin/env bash

# Assumes the version of weesu meant to be tested is on the PATH

set -eouC pipefail

cd $(dirname "$0")

CATALOG1=$(mktemp)
CATALOG2=$(mktemp)
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

echo Tests passed\!
