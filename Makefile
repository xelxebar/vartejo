SRCDIR=$(PWD)/src
BINDIR=$(PWD)/bin
PROJECTS=anki                \
	 chroot-qcow2        \
	 json-cmd            \
	 qemu-mac            \
	 randwipe            \
	 synclient-toggle    \
	 xmppcat             \
	 xscreensaver-toggle \
	 browser-watch


install: $(patsubst %,install-%,$(PROJECTS))

install-%:
	[[ -f $(BINDIR) ]] || $(MAKE) preinstall
	$(MAKE) BINDIR=$(BINDIR) -C $(SRCDIR)/$* install

preinstall:
	mkdir -p $(BINDIR)

clean: $(patsubst %,clean-%,$(PROJECTS))
	rmdir $(BINDIR)
	
clean-%:
	rm $(BINDIR)/$*
