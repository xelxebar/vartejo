#!/usr/bin/env sh

obj=$(mktemp -u)
exe=$(mktemp -u)

as "${1}" -o "${obj}"
ld "${obj}" -o "${exe}"
"${exe}"

rm "${obj}" "${exe}"
