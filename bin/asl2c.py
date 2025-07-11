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
:filter_reachable_from --keep-builtins exports

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

{xform_int_bitslices}

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
:filter_reachable_from --keep-builtins exports

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

// Lift let-expressions as high as possible out of an expression
// e.g., F(G(let t = 1 in H(t))) -> let t = 1 in F(G(H(t)))
// (This makes later transformations work better if, for example,
// they should match against "G(H(..))")
//
// Note that source code is not expected to contain let-expressions.
// They only exist to make some transformations easier to write.
:xform_hoist_lets

// Convert bitslice operations like "x[i] = '1';" to a combination
// of AND/OR and shift operations like "x = x OR (1 << i);"
// This works better after constant propagation/monomorphization.
:xform_bitslices {suppress_bitslice_xform}

// Any case statement that does not correspond to what the C language
// supports is converted to an if statement.
// This works better after constant propagation/monomorphization
// because that can eliminate/simplify guards on the clauses of the case statement.
:xform_case

{wrap_variables}

// A final pass of constant propagation simplifies any constant expressions
// introduced by previous transforms
:xform_constprop --nounroll

// Optimization: optionally use :xform_bounded to represent any
// constrained integers by an integer that is exactly the right size
// to contain it.
// This should come at the end of all the transformations because it changes
// the types of functions.
{bounded_int}

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
// Optionally, global state can be split across multiple structs.
// (This can be useful when modelling multi-processor systems to separate
// thread-local state from global state.)
:{generate_c}

:quit
""".strip()

################################################################
# Support functions
################################################################

verbose = False

def report(x):
    if verbose:
        print(x)

# Run command (printing command first if verbose) and abort if command fails
# (The assumption is that the command printed a useful/meaningful error message already)
def run(cmd):
    report(" ".join(cmd))
    try:
        subprocess.run(cmd, check=True)
    except subprocess.CalledProcessError as e:
        print(f"Command '{' '.join(e.cmd)}' returned non-zero exit status {e.returncode}.")
        sys.exit(e.returncode)

################################################################
# Compile/link flags
################################################################

ac_types_dir = os.environ.get('AC_TYPES_DIR')
ac_types_include = [f"-I{ac_types_dir}/include"] if ac_types_dir else []

sc_types_dir = os.environ.get('SC_TYPES_DIR')
sc_types_include = [f"-I{sc_types_dir}/include"] if sc_types_dir else []

backend_c_flags = {
    'ac':          ['-DASL_AC'] + ac_types_include,
    'c23':         ['-DASL_C23'],
    'interpreter': [],
    'fallback':    ['-DASL_FALLBACK'],
    'sc':          ['-DASL_SC'] + sc_types_include,
}

def get_c_flags(asli, backend):
    if "opam" in asli:
        # ASLi has been installed: query it for flags
        c_flags = subprocess.check_output([asli, "--print-c-flags"]).decode('utf-8').strip().split()
    else:
        # ASLi has not been installed so let's assume that it is being run
        # directly out of the build tree and the path looks like this ../_build/install/default/bin/asli
        # and the include path that we need is ../_build/install/default/runtime/include
        bindir = os.path.dirname(asli)
        rootdir = os.path.dirname(bindir)
        path = os.path.join(rootdir, "lib/asli/runtime_include")
        c_flags = [f"-I{path}"]
    c_flags.extend(backend_c_flags[backend])
    return c_flags

backend_ld_flags = {
    'ac':          [],
    'c23':         [],
    'interpreter': [],
    'fallback':    [],
    'sc':          ["-lsystemc"],
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
        path = os.path.join(rootdir, "lib/asli/runtime/libASL.a")
        ld_flags = [path]
    if backend == "sc":
        sc_types_dir = os.environ.get('SC_TYPES_DIR')
        if not sc_types_dir:
            raise EnvironmentError("SC_TYPES_DIR environment variable must be set for SystemC backend")
        ld_flags.append(f"-L{sc_types_dir}/lib")
    ld_flags.extend(backend_ld_flags[backend])
    return ld_flags

################################################################
# Script generation
################################################################

def mk_script(args, output_directory):
    # when using the --build/--run flags, we use the new FFI
    ffi = "--new-ffi" if args.build or args.new_ffi else ""

    backend_generator = {
        'ac':          f'generate_c {ffi} --runtime=ac',
        'c23':         f'generate_c {ffi} --runtime=c23',
        'fallback':    f'generate_c {ffi} --runtime=fallback',
        'sc':          f'generate_c {ffi} --runtime=sc',
        'mlir':        f'generate_mlir',
    }
    generate_c = backend_generator[args.backend]

    if args.const_ref: generate_c += f" --const-ref={args.const_ref}"
    if args.generate_cxx: generate_c += " --generate-cxx"
    if args.split_state: generate_c += " --split-state"
    if args.backend != 'mlir':
        generate_c += f" --output-dir={output_directory}"
        generate_c += f" --basename={args.basename}"
        generate_c += f" --num-c-files={args.num_c_files}"
        if args.line_info:
            generate_c += " --line-info"
        else:
            generate_c += " --no-line-info"
    else:
        generate_c += f" --output-file={output_directory}/asl.mlir"

    if args.O0:
        script = []
        script.append(":filter_unlisted_functions imports")
        script.append(":filter_reachable_from exports")
        if args.Obounded: script.append(":xform_bounded")
        if args.show_final_asl:
            script.append(f":show --format=raw")
        else:
            script.append(f":{backend_generator[args.backend]} --output-dir={output_directory} --basename={args.basename} --num-c-files=1")
        return "\n".join(script)

    substitutions = {
        'command':     " ".join(sys.argv),
        'generate_c':  generate_c,
        'split_state': "",
        'suppress_bitslice_xform': "",
        'xform_int_bitslices': "",
        'track_valid': "",
        'wrap_variables': "",
    }
    if args.instrument_unknown: substitutions['track_valid'] = ":xform_valid track-valid"
    if args.transform_int_slices:
        substitutions["xform_int_bitslices"] = textwrap.dedent("""\
            // Eliminate slices of integers by first converting the integer to a bitvector.
            // e.g., if "x : integer", then "x[1 +: 8]" to "cvt_int_bits(x, 9)[1 +: 8]"
            :xform_int_bitslices
            """)
    if args.wrap_variables: substitutions['wrap_variables'] = ":xform_wrap"
    if not args.auto_case_split:
        substitutions['auto_case_split'] = '--no-auto-case-split'
    else:
        substitutions['auto_case_split'] = '--auto-case-split'
    if args.Obounded:
        substitutions['bounded_int'] = ':xform_bounded'
    else:
        substitutions['bounded_int'] = ''
    if args.backend in ["ac", "sc"]: substitutions['suppress_bitslice_xform'] = "--notransform"

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

def mk_filenames(backend, working_directory, basename, use_cxx):
    project_file = f"{working_directory}/asl2c.prj"
    config_file = f"{working_directory}/config.json"
    suffix = "cpp" if use_cxx else "c"
    c_files = [
        f"{working_directory}/{basename}_exceptions.{suffix}",
        f"{working_directory}/{basename}_vars.{suffix}",
        f"{working_directory}/{basename}_funs.{suffix}"
    ]
    exe_file = f"{working_directory}/{basename}"
    return (project_file, config_file, c_files, exe_file)

def generate_project(project_file, script):
    with open(project_file, "w") as f:
        print(script, file=f)
    report(f"# Generated project {project_file}")

def generate_config_file(config_file, exports, imports):
    exports = ",".join([ f'"{x}"' for x in exports ])
    imports = ",".join([ f'"{x}"' for x in imports ])
    with open(config_file, "w") as f:
        print("{", file=f)
        print(f'  "exports": [{exports}],', file=f)
        print(f'  "imports": [{imports}]', file=f)
        print("}", file=f)
    report(f"# Generated configuration file {config_file}\n")

def run_asli(asli, args, asl_files, project_file, configurations):
    asli_cmd = [
        asli,
        "--batchmode", "--nobanner",
    ]
    asli_cmd.append("--check-call-markers")
    asli_cmd.append("--check-exception-markers")
    asli_cmd.append("--check-constraints" if args.constraint_checks else "--no-check-constraints")
    asli_cmd.append("--runtime-checks" if args.runtime_checks else "--no-runtime-checks")
    if args.Obounded: asli_cmd.append("--exec=:xform_bounded")
    asli_cmd.append(f"--project={project_file}")
    for file in configurations:
        asli_cmd.append(f"--configuration={file}")
    asli_cmd.extend(asl_files)
    run(asli_cmd)

def compile_and_link(use_cxx, c_files, extra_c, exe_file, working_directory, c_flags, ld_flags):
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
        elif use_cxx:
            cc = [ "g++" ]
        else:
            cc = [ "gcc" ]
    if use_cxx:
        # ac_types_dir = os.environ.get("AC_TYPES_DIR")
        # if not ac_types_dir:
        #     print("Error: environment variable AC_TYPES_DIR must be set when using C++ backends")
        #     exit(1)
        # cc.append(f'-I{ac_types_dir}/include')
        ld_flags.append('-lstdc++')
        c_flags.append('-std=c++17')
    else:
        c_flags.append('-std=c2x')

    # compile extra C files
    extra_objs = []
    for c_file in extra_c:
        nm = os.path.basename(c_file)
        (nm, _) = os.path.splitext(nm)
        obj_file = f"{working_directory}/{nm}.o"
        cc_cmd = cc + [
            f"-I{working_directory}",
            "-c",
            "-o", obj_file,
            c_file
        ]
        run(cc_cmd)
        extra_objs.append(obj_file)

    cc_cmd = cc + [
        "-Wno-parentheses-equality",
        f"-I{working_directory}",
        "-o", exe_file,
    ] + c_flags + c_files + extra_objs + ld_flags
    run(cc_cmd)

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
    parser.add_argument("--configuration", help="compilation configuration files (only use with --build or --run)", metavar="json", action='append', default=[])
    parser.add_argument("--auto-case-split", help="generate case split code automatically", action=argparse.BooleanOptionalAction)
    parser.add_argument("--const-ref", help="use const & for function arguments larger than N", metavar="N", type=int, default=0)
    parser.add_argument("--constraint-checks", help="perform type contstraint checks", action=argparse.BooleanOptionalAction, default=False)
    parser.add_argument("--extra-c", help="extra C file to be compiled/linked with ASL code (C generation only)", action='append', default=[])
    parser.add_argument("--export", dest="exports", help="export this symbol (C generation only)", action='append', default=[])
    parser.add_argument("--generate-cxx", help="generate C++ code", action="store_true", default=False)
    parser.add_argument("--import", dest="imports", help="import this symbol (C generation only)", action='append', default=[])
    parser.add_argument("--line-info", help="insert line directives into C code", action=argparse.BooleanOptionalAction)
    parser.add_argument("--new-ffi", help="use the new FFI", action="store_true", default=False)
    parser.add_argument("--runtime-checks", help="perform runtime checks (array bounds, etc.)", action=argparse.BooleanOptionalAction, default=False)
    parser.add_argument("--split-state", help="split state into multiple structs", action=argparse.BooleanOptionalAction)
    parser.add_argument("--transform-int-slices", help="convert integer slices to bit slices", action=argparse.BooleanOptionalAction, default=False)
    parser.add_argument("--instrument-unknown", help="instrument assignments of UNKNOWN", action=argparse.BooleanOptionalAction)
    parser.add_argument("--wrap-variables", help="wrap global variables into functions", action=argparse.BooleanOptionalAction)
    parser.add_argument("-O0", help="perform minimal set of transformations", action=argparse.BooleanOptionalAction)
    parser.add_argument("-Obounded", help="enable integer bounding optimization", action="store_true", default=False)
    parser.add_argument("--backend", help="select backend (default: c23)", choices=['ac', 'c23', 'interpreter', 'fallback', 'mlir', 'sc'], default='c23')
    parser.add_argument("--print-c-flags", help="print the C flags needed to use the selected ASL C runtime", action=argparse.BooleanOptionalAction)
    parser.add_argument("--print-ld-flags", help="print the Linker flags needed to use the selected ASL C runtime", action=argparse.BooleanOptionalAction)
    parser.add_argument("--build", help="compile and link the ASL code", action='store_true')
    parser.add_argument("--run", help="compile, link and run the ASL code", action='store_true')
    parser.add_argument("--show-final-asl", help="stop after optimization, dump ASL", action=argparse.BooleanOptionalAction)
    parser.add_argument("--verbose", "-v", help="verbose", action='store_true')
    parser.add_argument("--working-dir", help="working directory, by default temporarily created", metavar="working_dir", default="")
    parser.add_argument("--save-temps", help="save intermediate compilation results", action='store_true')
    parser.add_argument('asl_files', nargs='*', metavar="<.asl file>", help="ASL input files (compile/link/run only)")
    args = parser.parse_args()

    verbose = args.verbose
    args.build = args.build or args.run or args.show_final_asl
    if args.asl_files and not args.build:
        print("Error: only provide input files if building or running")
        exit(1)
    if args.num_c_files != 1 and args.build:
        print("Error: don't specify --num-c-files if building or running")
        exit(1)
    if (args.imports or args.exports or args.extra_c) and not args.build:
        print("Error: don't specify --imports or --exports or --extra-c if not building or not running")
        exit(1)
    if args.configuration and not args.build:
        print("Error: don't specify --configuration unless building or running")
        exit(1)
    if args.backend == "interpreter" and not args.run:
        print("Error: must specify --run with asli backend")
        exit(1)
    if args.backend in ['ac', 'sc']: args.generate_cxx = True
    if args.const_ref and not args.generate_cxx:
        print("Error: must specify --generate-cxx with --const-ref")
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
        ]
        asli_cmd.append("--check-call-markers")
        asli_cmd.append("--check-exception-markers")
        asli_cmd.append("--check-constraints" if args.constraint_checks else "--no-check-constraints")
        asli_cmd.append("--runtime-checks" if args.runtime_checks else "--no-runtime-checks")
        if args.Obounded: asli_cmd.append("--exec=:xform_bounded")
        asli_cmd.extend([
            "--exec=let result = main();",
            "--exec=:quit",
        ])
        asli_cmd.extend(args.asl_files)
        run(asli_cmd)
    elif args.run and args.backend == "mlir":
        working_directory = make_working_dir(args.working_dir, prefix="asltest.")
        config_file = f"{working_directory}/config.json"
        mlir_file = f"{working_directory}/asl.mlir"
        asli_cmd = [
            asli,
            "--batchmode",
            f"--configuration={config_file}",
            "--exec=:filter_reachable_from exports",
            f"--exec=:generate_mlir --output-file={mlir_file}",
            "--exec=:quit",
        ]
        asli_cmd.extend(args.asl_files)
        generate_config_file(config_file, ["main"] + args.exports, args.imports)
        run(asli_cmd)
        print(f"# Generated {mlir_file}")
        mlir_cmd = ["asl-opt", "--target=exec", mlir_file]
        run(mlir_cmd) 
        print(f"# Ran {mlir_file}")
        if not args.save_temps: shutil.rmtree(working_directory)
    else:
        backend = args.backend
        working_directory = make_working_dir(args.working_dir, prefix="asltest.")
        report(f"# In temporary directory {working_directory}")
        script = mk_script(args, working_directory)
        (project_file, config_filename, c_files, exe_file) = mk_filenames(backend, working_directory, args.basename, args.generate_cxx)
        generate_project(project_file, script)
        generate_config_file(config_filename, ["main"] + args.exports, args.imports)
        run_asli(asli, args, args.asl_files, project_file, [config_filename]+args.configuration)
        if args.show_final_asl:
            pass
        elif args.run:
            c_flags = get_c_flags(asli, backend)
            ld_flags = get_ld_flags(asli, backend)
            compile_and_link(args.generate_cxx, c_files, args.extra_c, exe_file, working_directory, c_flags, ld_flags)
            run([exe_file])
        if not args.save_temps: shutil.rmtree(working_directory)

if __name__ == "__main__":
    main()

################################################################
# End
################################################################
