################################################################
# Demo Makefile
#
# Copyright (C) 2022-2025 Intel Corporation
# SPDX-Licence-Identifier: BSD-3-Clause
################################################################

.default: test

ASL_PATH = ..:.
ASLI     = ../_build/install/default/bin/asli
ASL2C    = ../_build/install/default/bin/asl2c

################################################################
# Test whether we have GNU as or clang emulating as
# (We can only build an elf file and run the test if we have
# GNU as.)
################################################################

HAVE_GNU_AS := `as --version | grep -q 'GNU assembler'`

REPORT_NOT_GAS = "Unable to build ELF file to use in demo: GNU 'as' required - skipping demo"

################################################################
# Build a test program using the demo ISA
################################################################

clean::
	$(RM) test.o test.elf

# run preprocessor on test.S using default rule
test.s: test.S assembly.s

test.o: test.s
	@ if ${HAVE_GNU_AS}; then \
	as test.s -o test.o; \
	else echo ${REPORT_NOT_GAS}; fi

test.elf: test.o
	@ if ${HAVE_GNU_AS}; then \
	ld test.o -o test.elf; \
	nm test.elf; \
	else echo ${REPORT_NOT_GAS}; fi

################################################################
# Run demo on interpreter
################################################################

test: test.elf test.prj
	@ if ${HAVE_GNU_AS}; then \
	env ASL_PATH="${ASL_PATH}" ${ASLI} --nobanner --batchmode demo.asl --project test.prj | filecheck test.prj ; \
	else echo ${REPORT_NOT_GAS}; fi

demo: test.elf
	@ if ${HAVE_GNU_AS}; then \
	env ASL_PATH="${ASL_PATH}" ${ASLI} --batchmode demo.asl --project test.prj ; \
	else echo ${REPORT_NOT_GAS}; fi

################################################################
# Build simulator using C backend
################################################################

CFLAGS += -Wall
CFLAGS += -g
CFLAGS += -Wno-unused-but-set-variable

CC=clang-16 -std=c2x

.PRECIOUS: simulator_%

simulator_% : simulator.c config.json demo.asl
	@ if ${HAVE_GNU_AS}; then \
	$(ASL2C) --basename=sim --intermediates=log --split-state --new-ffi --backend=$* > sim.prj ; \
	env ASL_PATH="${ASL_PATH}" $(ASLI) --nobanner --batchmode --project=sim.prj --configuration=config.json demo.asl ; \
	$(CC) ${shell ${ASL2C} --backend=$* --print-c-flags} ${CFLAGS} simulator.c -o $@ ${shell ${ASL2C} --backend=$* --print-ld-flags} ${LDFLAGS} ; \
	else echo ${REPORT_NOT_GAS}; fi

test_% : simulator_% test.elf test.prj
	@ if ${HAVE_GNU_AS}; then \
	./simulator_$* test.elf --steps=20 | filecheck test.prj ; \
	else echo ${REPORT_NOT_GAS}; fi

demo_% : simulator_% test.elf test.prj
	@ if ${HAVE_GNU_AS}; then \
	./simulator_$* test.elf --steps=20; \
	else echo ${REPORT_NOT_GAS}; fi

clean ::
	$(RM) sim.prj
	$(RM) log.*.asl
	$(RM) sim_types.h sim_exceptions.h sim_vars.h sim_ffi.h
	$(RM) sim_exceptions.c sim_vars.c sim_funs.c
	$(RM) sim_types.hpp sim_exceptions.hpp sim_vars.hpp
	$(RM) sim_exceptions.cpp sim_vars.cpp sim_funs.cpp
	$(RM) simulator_*

################################################################
# End
################################################################
