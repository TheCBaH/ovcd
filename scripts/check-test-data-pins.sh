#!/bin/sh
# Fail if an extra-source commit in ovcd.opam differs from the test-data gitlink.
cd "$(dirname "$0")/.." || exit 2
status=0
for path in $(git ls-tree HEAD test-data/ | awk '{print $4}'); do
  name=${path#test-data/}
  want=$(git ls-tree HEAD "$path" | awk '{print $3}')
  have=$(sed -n "s|.*/$name/archive/\([0-9a-f]\{40\}\)\.tar\.gz.*|\1|p" ovcd.opam)
  if [ "$want" != "$have" ]; then
    echo "DRIFT $name: gitlink ${want:-none}, extra-source ${have:-none}"
    status=1
  else
    echo "ok $name $want"
  fi
done
exit $status
