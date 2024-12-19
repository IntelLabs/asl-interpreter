#! /usr/bin/env python3

"""
Generate asli script to generate C code from ASL

Typical usage:

    mkdir genc
    bin/asl2c.py --output-dir=genc --basename=sim --intermediates=genc/log --line-info --num-c-files=8 > genc/asl2c.prj
    asli --batchmode --nobanner --project=genc/asl2c.prj --configuration=imports.json --configuration=exports.json spec.asl

Alternative usage (compile and run the ASL function 'main' using backend fallback):

    bin/asl2c.py --backend=fallback --run spec.asl
"""

import argparse
import os
import os.path
import pathlib
import shutil
import string
import subprocess
import sys
import tempfile

################################################################
# The base script
################################################################

base_script = """
// Autogenerated script to generate C code from ASL
//
// Generated by command: {command}

// Discard any code not reachable from the list of exported functions.
// This is done early because it can remove code that we cannot compile.
//
// The list of exports is loaded using the "--configuration=foo.json" flag
// and an example json file that exports two functions called "Reset" and "Step"
// is as follows.
//
//     {{
//         "__comment": [
//             "Export the interface required by the simulator"
//         ],
//         "exports": [
//             "Reset",
//             "Step"
//         ]
//     }}
//
// The "__comment" part is optional and is ignored.
//
// Multiple configuration files can be loaded if you want to group the set of
// exports into multiple logical interfaces.
//
:filter_reachable_from exports

// A series of 'desugaring' passes eliminate features that complicate later transformations.
// Later transformations will fail if these features have not been removed.

// Eliminate 'typedef'
:xform_named_type

// Eliminate bit,int arithmetic operations like "'000' + 3"
:xform_desugar

// Eliminate bit-tuples like "[x,y] = z;" and "x[7:0, 15:8]";
:xform_bittuples

// Convert all bit-slice operations to use the +: syntax.
// e.g., "x[7:0]" --> "x[0 +: 8]"
:xform_lower

// Eliminate slices of integers by first converting the integer to a bitvector.
// e.g., if "x : integer", then "x[1 +: 8]" to "cvt_int_bits(x, 9)[1 +: 8]"
:xform_int_bitslices

// Convert use of getter/setter syntax to function calls.
// e.g., "Mem[a, sz] = x" --> "Mem_set(a, sz, x)"
:xform_getset

{track_valid}

// Perform constant propagation without unrolling loops
// This helps identify potential targets for the monomorphization pass.
:xform_constprop --nounroll

// Create specialized versions of every bitwidth-polymorphic function
// and change all function calls to use the appropriate specialized
// version.
// (Note that this performs an additional round of constant propagation.)
:xform_monomorphize {auto_case_split}

// Discard any code not reachable from the list of exported functions.
// This step is repeated because it deletes any bitwidth-polymorphic functions
// that have been completely replaced by specialized versions of the functions.
:filter_reachable_from exports

// todo: explain why this needs to be repeated
:xform_monomorphize {auto_case_split}

// Perform a further round of simplifying passes.
// This is done after performing monomorphization and constant propagation
// because they work better in code where bitwidths are constants
// or they make constant propagation less precise.

// Change any function that returns multiple results to return a
// record with multiple fields and change all calls to that function.
// (This makes constant propagation less precise because constant propagation
// is not field sensitive.)
:xform_tuples

// Convert use of getter/setter syntax to function calls.
// e.g., "Mem[a, sz] = x" --> "Mem_set(a, sz, x)"
// (This is repeated because xform_tuples can expose additional
// getter/setter calls.)
:xform_getset

// todo: explain why this needs to be repeated
:xform_bittuples

// Convert bitslice operations like "x[i] = '1';" to a combination
// of AND/OR and shift operations like "x = x OR (1 << i);"
// This works better after constant propagation/monomorphization.
:xform_bitslices

// Any case statement that does not correspond to what the C language
// supports is converted to an if statement.
// This works better after constant propagation/monomorphization
// because that can eliminate/simplify guards on the clauses of the case statement.
:xform_case

{wrap_variables}

// A final pass of constant propagation simplifies any constant expressions
// introduced by previous transforms
:xform_constprop --nounroll

// To let the generated code call your own functions, you need to declare
// the type of an ASL function with a matching type and provide a configuration
// file containing a list of these external functions.
// For example, if you want to track reads and writes to memory, you might
// use this import file (loaded using the --configuration flag)
//
//     {{
//         "__comment": [
//             "Import memory trace functions"
//         ],
//         "imports": [
//             "TraceMemRead",
//             "TraceMemWrite"
//         ]
//     }}
//
// If you have defined the behavior of these functions in ASL (e.g., for use in
// the ASL interpreter), you need to delete the ASL definitions of these
// functions.
:filter_unlisted_functions imports

// Remove global variables corresponding to thread-local variables
// (Thread local variables are listed in a configuration file like this
//
//     {{
//         "thread_local_state": [
//             "GPR",
//             "RFLAGS"
//         ]
//     }}
:filter_listed_variables thread_local_state

// Deleting the ASL definitions of any functions on the import list may
// result in additional dead code (i.e., functions that are only used by
// those functions) so delete any unreachable functions
:filter_reachable_from exports

// Check that all definitions are bitwidth-monomorphic and report a useful
// error message if they are not.
// The code generator will produce an error message if it finds a call
// to a polymorphic functions but we can produce a much more useful error message
// if we scan for all polymorphic functions and organize the list of functions
// into a call tree so that you can see which functions are at the roots of
// the tree (and therefore are the ones that you need to fix).
:check_monomorphization --fatal --verbose

// Generate C code from the remaining definitions
//
// This produces multiple files that will be prefixed by the basename and saved
// into the output-dir directory.
//
// Compilation of all the functions can be parallelized by splitting the
// file containing the functions into smaller files (using --num-c-files=<N>)
// and compiling them in parallel.
//
// Optionally, the C code can use #line directives so that profiling, debuggers,
// etc. know what file/line in the ASL file produced each line of C code.
// Use --line-info or --no-line-info to control this.
//
// Optionally, references to thread-local processor state such as registers
// can be accessed via a pointer.
// (This can be useful when modelling multi-processor systems.)
:{generate_c} --output-dir={output_dir} --basename={basename} --num-c-files={num_c_files} {line_info} {thread_local}

:quit
""".strip()

################################################################
# Support functions
################################################################

verbose = False

def report(x):
    if verbose:
        print(x)

def run(cmd):
    report(" ".join(cmd))
    subprocess.run(cmd, check=True)

################################################################
# Compile/link flags
################################################################

ac_types_dir = os.environ.get('AC_TYPES_DIR')
ac_types_include = [f"-I{ac_types_dir}/include"] if ac_types_dir else []

backend_c_flags = {
    'ac':          ['-DASL_AC'] + ac_types_include,
    'c23':         ['-DASL_C23'],
    'interpreter': [],
    'fallback':    ['-DASL_FALLBACK'],
    'orig':        ['-DASL_FALLBACK'],
}

def get_c_flags(asli, backend):
    if "opam" in asli:
        # ASLi has been installed: query it for flags
        c_flags = subprocess.check_output([asli, "--print-c-flags"]).decode('utf-8').strip().split()
    else:
        # ASLi has not been installed so let's assume that it is being run
        # directly out of the build tree and the path looks like this ../_build/default/bin/asli.exe
        # and the include path that we need is ../_build/default/runtime/include
        bindir = os.path.dirname(asli)
        rootdir = os.path.dirname(bindir)
        path = os.path.join(rootdir, "runtime/include")
        c_flags = [f"-I{path}"]
    c_flags.extend(backend_c_flags[backend])
    return c_flags

backend_ld_flags = {
    'ac':          [],
    'c23':         [],
    'interpreter': [],
    'fallback':    [],
    'orig':        [],
}

def get_ld_flags(asli, backend):
    if "opam" in asli:
        # ASLi has been installed: query it for flags
        ld_flags = subprocess.check_output([asli, "--print-ld-flags"]).decode('utf-8').strip().split()
    else:
        # ASLi has not been installed so let's assume that it is being run
        # directly out of the build tree and the path looks like this ../_build/install/default/bin/asli
        # and the include path that we need is ../_build/install/default/runtime
        bindir = os.path.dirname(asli)
        rootdir = os.path.dirname(bindir)
        path = os.path.join(rootdir, "runtime/libASL.a")
        ld_flags = [path]
    ld_flags.extend(backend_ld_flags[backend])
    return ld_flags

################################################################
# Script generation
################################################################

def mk_script(args, output_directory):
    # when using the --build/--run flags, we use the new FFI
    ffi = "--new-ffi" if args.build else ""

    backend_generator = {
        'ac':          f'generate_c_new {ffi} --runtime=ac',
        'c23':         f'generate_c_new {ffi} --runtime=c23',
        'fallback':    f'generate_c_new {ffi} --runtime=fallback',
        'orig':        'generate_c',
    }

    if args.O0:
        filter = ":filter_reachable_from exports"
        generate = f":{backend_generator[args.backend]} --output-dir={output_directory} --basename={args.basename} --num-c-files=1"
        script = [filter, generate]
        return "\n".join(script)

    substitutions = {
        'command':     " ".join(sys.argv),
        'basename':    args.basename,
        'generate_c':  backend_generator[args.backend],
        'line_info':   "",
        'num_c_files': args.num_c_files,
        'output_dir':  output_directory,
        'track_valid': "",
        'wrap_variables': "",
    }
    if args.instrument_unknown: substitutions['track_valid'] = ":xform_valid track-valid"
    if args.wrap_variables: substitutions['wrap_variables'] = ":xform_wrap"
    if not args.auto_case_split:
        substitutions['auto_case_split'] = '--no-auto-case-split'
    else:
        substitutions['auto_case_split'] = '--auto-case-split'
    if not args.line_info:
        substitutions['line_info'] = '--no-line-info'
    else:
        substitutions['line_info'] = '--line-info'
    if args.thread_local_pointer:
        thread_local = f"--thread-local-pointer={args.thread_local_pointer}"
        thread_local += f" --thread-local=thread_local_state"
    else:
        thread_local = ''
    substitutions['thread_local'] = thread_local

    script = base_script.format(**substitutions)

    if args.intermediates:
        lines_in = script.splitlines()
        lines_out = []
        i = 0
        for l in lines_in:
            if l.startswith(":"):
                command = l.split()[0][1:]
                lines_out.append(f":show --format=raw --output {args.intermediates}.{i:02}.{command}.asl")
                i = i + 1
            lines_out.append(l)

        script = "\n".join(lines_out)

    return script

################################################################
# Building support functions
################################################################

def mk_filenames(backend, working_directory, basename):
    project_file = f"{working_directory}/asl2c.prj"
    exports_file = f"{working_directory}/exports.json"
    suffix = "cpp" if backend in ["ac"] else "c"
    c_files = [
        f"{working_directory}/{basename}_exceptions.{suffix}",
        f"{working_directory}/{basename}_vars.{suffix}",
        f"{working_directory}/{basename}_funs.{suffix}"
    ]
    exe_file = f"{working_directory}/{basename}"
    return (project_file, exports_file, c_files, exe_file)

def generate_project(project_file, script):
    with open(project_file, "w") as f:
        print(script, file=f)
    report(f"# Generated project {project_file}\n")

def generate_exports(export_file):
    with open(export_file, "w") as f:
        print("{\"exports\": [\"main\"]}\n", file=f)
    report(f"# Generated export configuration file {export_file}\n")

def generate_c(asli, asl_files, project_file, exports_file):
    asli_cmd = [
        asli,
        "--batchmode", "--nobanner",
        f"--project={project_file}",
        f"--configuration={exports_file}"
    ]
    asli_cmd.extend(asl_files)
    run(asli_cmd)

def compile_and_link(use_cxx, c_files, exe_file, include_directory, c_flags, ld_flags):
    cc = os.environ.get("CC")
    if cc:
        cc = cc.split()
    else:
        if subprocess.run(['which', 'clang-18'], capture_output=True).returncode == 0:
            cc = [ "clang-18" ]
        elif subprocess.run(['which', 'clang-17'], capture_output=True).returncode == 0:
            cc = [ "clang-17" ]
        elif subprocess.run(['which', 'clang-16'], capture_output=True).returncode == 0:
            cc = [ "clang-16" ]
        elif subprocess.run(['which', 'clang'], capture_output=True).returncode == 0:
            cc = [ "clang" ]
        else:
            cc = [ "gcc" ]
    if use_cxx:
        # ac_types_dir = os.environ.get("AC_TYPES_DIR")
        # if not ac_types_dir:
        #     print("Error: environment variable AC_TYPES_DIR must be set when using C++ backends")
        #     exit(1)
        # cc.append(f'-I{ac_types_dir}/include')
        cc.append('-lstdc++')
    else:
        cc.append('-std=c2x')
    cc_cmd = cc + [
        f"-I{include_directory}",
        "-o", exe_file,
    ] + c_flags + c_files + ld_flags
    run(cc_cmd)

def build(script, asl_files, asli, backend, working_directory, basename):
    (project_file, exports_file, c_files, exe_file) = mk_filenames(backend, working_directory, basename)
    c_flags = get_c_flags(asli, backend)
    ld_flags = get_ld_flags(asli, backend)
    generate_project(project_file, script)
    generate_exports(exports_file)
    generate_c(asli, asl_files, project_file, exports_file)
    use_cxx = backend in ['ac']
    compile_and_link(use_cxx, c_files, exe_file, working_directory, c_flags, ld_flags)
    return exe_file

def make_working_dir(name, prefix=""):
    if name:
        os.makedirs(name, exist_ok=True)
    else:
        name = tempfile.mkdtemp(prefix=prefix)
    report(f"# Created temporary directory {name}")
    return name

################################################################
# Main
################################################################

def main() -> int:
    global verbose

    parser = argparse.ArgumentParser(
            prog = 'asl2c',
            description = __doc__,
            formatter_class=argparse.RawDescriptionHelpFormatter,
            )
    parser.add_argument("--intermediates", help="generate intermediate files with prefix", metavar="log_prefix")
    parser.add_argument("--output-dir", help="output directory for generated files", metavar="output_dir", default="")
    parser.add_argument("--basename", help="basename of generated C files (default: \"asl\")", metavar="output_prefix", default="asl")
    parser.add_argument("--num-c-files", help="write functions to N files (default: 1)", metavar="N", type=int, default=1)
    parser.add_argument("--auto-case-split", help="generate case split code automatically", action=argparse.BooleanOptionalAction)
    parser.add_argument("--line-info", help="insert line directives into C code", action=argparse.BooleanOptionalAction)
    parser.add_argument("--thread-local-pointer", help="name of pointer to thread-local processor state", metavar="varname", default=None)
    parser.add_argument("--instrument-unknown", help="instrument assignments of UNKNOWN", action=argparse.BooleanOptionalAction)
    parser.add_argument("--wrap-variables", help="wrap global variables into functions", action=argparse.BooleanOptionalAction)
    parser.add_argument("-O0", help="perform minimal set of transformations", action=argparse.BooleanOptionalAction)
    parser.add_argument("--backend", help="select backend (default: orig)", choices=['ac', 'c23', 'interpreter', 'fallback', 'orig'], default='orig')
    parser.add_argument("--print-c-flags", help="print the C flags needed to use the selected ASL C runtime", action=argparse.BooleanOptionalAction)
    parser.add_argument("--print-ld-flags", help="print the Linker flags needed to use the selected ASL C runtime", action=argparse.BooleanOptionalAction)
    parser.add_argument("--build", help="compile and link the ASL code", action='store_true')
    parser.add_argument("--run", help="compile, link and run the ASL code", action='store_true')
    parser.add_argument("--verbose", "-v", help="verbose", action='store_true')
    parser.add_argument("--working-dir", help="working directory, by default temporarily created", metavar="working_dir", default="")
    parser.add_argument("--save-temps", help="save intermediate compilation results", action='store_true')
    parser.add_argument('asl_files', nargs='*', metavar="<.asl file>", help="ASL input files (compile/link/run only)")
    args = parser.parse_args()

    verbose = args.verbose
    args.build = args.build or args.run
    if args.asl_files and not args.build:
        print("Error: only provide input files if building or running")
        exit(1)
    if args.num_c_files != 1 and args.build:
        print("Error: don't specify --num-c-files if building or running")
        exit(1)
    if args.backend == "interpreter" and not args.run:
        print("Error: must specify --run with asli backend")
        exit(1)

    # when running tests, we need to be able to use ASLi without having installed it
    asli = pathlib.Path(__file__).parent / "asli"
    if not asli.exists():
        asli = pathlib.Path(__file__).parent / "asli.exe"
    asli = str(asli)

    if args.print_c_flags:
        print(' '.join(get_c_flags(asli, args.backend)))
    elif args.print_ld_flags:
        print(' '.join(get_ld_flags(asli, args.backend)))
    elif not args.build:
        print(mk_script(args, args.output_dir))
    elif args.run and args.backend == "interpreter":
        asli_cmd = [
            asli,
            "--batchmode", "--nobanner",
            "--exec=let result = main();",
            "--exec=:quit",
        ]
        asli_cmd.extend(args.asl_files)
        run(asli_cmd)
    else:
        working_directory = make_working_dir(args.working_dir, prefix="asltest.")
        report(f"# In temporary directory {working_directory}")
        script = mk_script(args, working_directory)
        exe_file = build(script, args.asl_files, asli, args.backend, working_directory, args.basename)
        if args.run:
            run([exe_file])
        if not args.save_temps: shutil.rmtree(working_directory)

if __name__ == "__main__":
    main()

################################################################
# End
################################################################