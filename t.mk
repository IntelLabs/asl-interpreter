all: test.ext

VDIR = /usr/share/verilator/include

test.exe: obj_dir/Vt__ALL.a
	g++ -Iobj_dir -I${VDIR} test.cc ${VDIR}/verilated.cpp obj_dir/Vt__ALL.a -o test.exe

obj_dir/Vt__ALL.a: t.v
	verilator --cc t.v test.v
	make -C obj_dir/ -f Vt.mk

clean::
	${RM} -r obj_dir
	${RM} test.exe

# End
