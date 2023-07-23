dmp ?= src.dmp
sys ?= src.sys

sforth: $(dmp)
	sed 's/#.*//' "$<" \
	| xxd -r -p - \
	| cat - "$(sys)" >"$@"
	chmod ug+x "$@"
