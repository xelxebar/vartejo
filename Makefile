src = $(PWD)/src
BIN = ~/.local/bin
targets      = $(notdir $(wildcard $(src)/*))
clean-targets = $(patsubst %, clean-%, $(targets))


all: $(targets)

$(targets): %: | $(BIN)/
	$(MAKE) BIN=$(BIN) -C $(src)/$* install

$(BIN)/:
	mkdir -p $(BIN)

clean: $(clean-targets)
	rmdir $(BIN)

$(clean-targets): clean-%:
	rm $(BIN)/$*
