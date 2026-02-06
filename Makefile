.PHONY: build clean run install uninstall test help

# Executable name
BINARY = shout

# Installation directory
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin

# Build the executable (as current user, quicklisp available)
build: $(BINARY)

$(BINARY): build.lisp shout.asd src/*.lisp
	sbcl --non-interactive --load build.lisp

# Run from source (development)
run:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(push #p\"$(PWD)/../multiposter/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :shout)" \
	     --eval "(shout:shout)"

# Run the built executable
run-bin: $(BINARY)
	./$(BINARY)

# Install to system (no quicklisp needed - just copies the binary)
install: $(BINARY)
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 $(BINARY) $(DESTDIR)$(BINDIR)/

# Uninstall from system
uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(BINARY)

# Clean build artifacts
clean:
	rm -f $(BINARY)
	rm -rf ~/.cache/common-lisp/sbcl-*/**/shout/

# Load into REPL for development
repl:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(push #p\"$(PWD)/../multiposter/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :shout)"

# Check syntax without running
check:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(push #p\"$(PWD)/../multiposter/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :shout)" \
	     --eval "(sb-ext:exit)"

help:
	@echo "SHOUT - Social Herald Over Unix Terminals"
	@echo ""
	@echo "Targets:"
	@echo "  build     - Build standalone executable (requires Quicklisp)"
	@echo "  run       - Run from source (requires Quicklisp)"
	@echo "  run-bin   - Run the built executable"
	@echo "  install   - Install to $(BINDIR) (just copies binary, no Quicklisp needed)"
	@echo "  uninstall - Remove from $(BINDIR)"
	@echo "  clean     - Remove build artifacts"
	@echo "  repl      - Start SBCL with SHOUT loaded"
	@echo "  check     - Check if code compiles"
	@echo "  help      - Show this help"
