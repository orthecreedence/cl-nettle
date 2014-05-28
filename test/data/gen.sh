#!/bin/bash

pushd hash >/dev/null
for x in `ls --color=never | grep -v '\.'`; do
	md5sum $x | sed 's| .*||' > $x.md5
	sha1sum $x | sed 's| .*||' > $x.sha1
	sha256sum $x | sed 's| .*||' > $x.sha256
	sha512sum $x | sed 's| .*||' > $x.sha512
done
popd >/dev/null

