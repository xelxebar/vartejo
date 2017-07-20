#!/usr/bin/env sh

src="${1}"
obj="$(mktemp -u)"
exe="$(mktemp -u)"

as "${src}" -o "${obj}"
ld "${obj}" -o "${exe}"
"${exe}"

rm "${obj}" "${exe}"
