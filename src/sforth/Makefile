dmp ?= src.dmp
sys ?= src.sys
ldt ?= link.ld

sforth: $(dmp)
	sed 's/#.*//' "$<" \
	| xxd -r -p - \
	| cat - "$(sys)" >"$@"
	chmod ug+x "$@"

sforth-asm: sforth-asm.o
	ld -o "$@" "$<"

sforth-asm.o: sforth-asm.S
	as -o "$@" "$<"

sforth-asm.S: $(dmp) $(sys)
	sed '1,/^### Code/d    # Code starts at _entry                        \
	     /^99/{            # Bytes are raw hex values                     \
	         s/^/.byte /                                                 #\
	         s/\([[:xdigit:]]\{2\}\) /0x\1,/g                            #\
	         s/\(0x[[:xdigit:]]\{2\}\),\( \+#\)/\1\2/                    #\
	         /Call/s/^/\t/                                               #\
	         b                                                           #\
	     }                                                               #\
	     /^# /s///         # Left-adjusted comments are code              \
	     s/^[^#]* # /\t/   # Opcodes are followed by instruction comments \
	     ' "$<" >"$@"

.PHONY: clean
clean: ; git clean -xf
