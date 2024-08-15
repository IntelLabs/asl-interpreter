################################################################
# ASL Makefile
#
# Copyright Arm Limited (c) 2017-2019
# Copyright (C) 2022-2024 Intel Corporation
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
test: test_backends

dune_test: build
	$(DUNE) test

runtime_test:
	$(MAKE) -C runtime test BUILD_DIR=$(DUNE_BUILD_DIR)/runtime

lit_test: build
	env PATH="`pwd`/tests/scripts:${PATH}" ${LIT} tests/lit -v

BACKENDS = interpreter c23 ac fallback

test_backends: ${addprefix test_backend_, ${BACKENDS}}

test_backend_%: build
	env PATH="${CURDIR}/tests/scripts:$${PATH}" AC_TYPES_DIR="`pwd`/ac_types-4.0" ASL_BACKEND=$* ${LIT} tests/backends -v

################################################################
# End
################################################################
