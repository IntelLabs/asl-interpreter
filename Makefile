################################################################
# ASL Makefile
#
# Copyright Arm Limited (c) 2017-2019
# Copyright Intel Inc (c) 2022-2023
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.DEFAULT: all

VERSION = 0.2.0

ifneq ($(V),1)
MAKEFLAGS += --silent
endif

export DUNE_BUILD_DIR ?= $(CURDIR)/_build

DUNE := dune
OPAM := opam
LIT := lit

build::
	$(DUNE) build
	$(MAKE) -C runtime BUILD_DIR=$(DUNE_BUILD_DIR)/runtime

install::
	$(DUNE) build @install
	$(DUNE) install

uninstall::
	$(DUNE) build @install
	$(DUNE) uninstall

publish::
	$(DUNE) build @install
	$(OPAM) publish https://github.com/alastairreid/asl-interpreter/archive/$(VERSION).tar.gz

doc::
	$(DUNE) build @doc
	@echo Documentation is in _build/default/_doc/_html/index.html

clean::
	$(RM) *~
	$(DUNE) clean

test: dune_test
test: runtime_test
test: lit_test

dune_test: build
	$(DUNE) test

runtime_test:
	$(MAKE) -C runtime test BUILD_DIR=$(DUNE_BUILD_DIR)/runtime

lit_test: build
	env PATH="`pwd`/tests/scripts:${PATH}" ${LIT} tests/lit -v

################################################################
# End
################################################################
