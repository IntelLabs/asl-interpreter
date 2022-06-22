#include <stdlib.h>
#include "Vt.h"
#include "verilated.h"

int main(int argc, char **argv) {
        Verilated::commandArgs(argc, argv);
        Vt *tb = new Vt;
        tb->clk = 0;
        tb->eval();
        tb->clk = 1;
        tb->eval();
        exit(EXIT_SUCCESS);
}
