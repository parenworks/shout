.PHONY: build build-ccl clean run run-ccl install uninstall test help

# Executable name
BINARY = shout

# Lisp implementation (default: sbcl)
LISP ?= sbcl

# Installation directory
PREFIX ?= /usr/local
BINDIR = $(PREFIX)/bin

# Build with SBCL (default)
build: $(BINARY)

$(BINARY): build.lisp shout.asd src/*.lisp
	sbcl --non-interactive --load build.lisp

# Build with CCL
build-ccl: build.lisp shout.asd src/*.lisp
	ccl --load build.lisp --eval '(quit)'

# Run from source (SBCL)
run:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(push #p\"$(PWD)/../multiposter/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :shout)" \
	     --eval "(shout:shout)"

# Run from source (CCL)
run-ccl:
	ccl --eval "(require :asdf)" \
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
	rm -rf ~/.ccl-cache/**/shout/

# Load into REPL for development (SBCL)
repl:
	sbcl --eval "(require :asdf)" \
	     --eval "(load \"~/quicklisp/setup.lisp\")" \
	     --eval "(push #p\"$(PWD)/\" asdf:*central-registry*)" \
	     --eval "(push #p\"$(PWD)/../multiposter/\" asdf:*central-registry*)" \
	     --eval "(ql:quickload :shout)"

# Load into REPL for development (CCL)
repl-ccl:
	ccl --eval "(require :asdf)" \
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
	@echo "  build      - Build executable with SBCL (default)"
	@echo "  build-ccl  - Build executable with CCL"
	@echo "  run        - Run from source with SBCL"
	@echo "  run-ccl    - Run from source with CCL"
	@echo "  run-bin    - Run the built executable"
	@echo "  install    - Install to $(BINDIR) (just copies binary)"
	@echo "  uninstall  - Remove from $(BINDIR)"
	@echo "  clean      - Remove build artifacts"
	@echo "  repl       - Start SBCL REPL with SHOUT loaded"
	@echo "  repl-ccl   - Start CCL REPL with SHOUT loaded"
	@echo "  check      - Check if code compiles (SBCL)"
	@echo "  help       - Show this help"
