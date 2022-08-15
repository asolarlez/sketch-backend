# Welcome to HyperSketch!!!

## Set up

### Sketch Set Up

[//]: # (~~~)
    mkdir sketch-source
    cd sketch-source
    git clone https://github.com/asolarlez/sketch-frontend.git   
    git clone https://github.com/asolarlez/sketch-backend.git
    cd sketch-backend
    bash autogen.sh
    ./configure
    make -j16
    <if fail due to C++17 not allowing 'register' specifier>
    grep -ilr 'register ' * | xargs -I@ sed -i '' 's/register //g' @
    make -j16
    <endif>
    cd ../sketch-frontend
    make run-local-seq EXEC_ARGS="src/test/sk/seq/miniTest1.sk"

[//]: # (~~~)

### HyperSketch Set Up

Manually:

    goto src/SketchSolver/InputParser/CommandLineArgs.h and change
    [[ string hypersketch_file_path = "" ]] 
    ->
    [[ string hypersketch_file_path = "my-hypersketch.hsk" ]]

then in the command line:
    
    make -j2

and then you can run the frontend, and it will execute the hypersketch program.
to confirm that hypersketch had run, at the end there should be .fmtl file created as well as a hypersketch_console.out file.

[//]: # (## Hello World example)

[//]: # (```)

[//]: # (hypersketch main&#40;&#41; {)

[//]: # (    print&#40;"Hello World!"&#41;;)

[//]: # (})

[//]: # (```)

[//]: # ()
[//]: # (hypersketch_console.out:)

[//]: # (```)

[//]: # (Hello World!)

[//]: # (```)

## HyperSketch Hello World: Running CEGIS

Synthesis checklist:
1. *.sk file, a sketch file where you ecode your program space.
2. [optional] *.data file, a data file where you provide inputs for the sketch harness.
3. *.hsk file, a hypersketch file where you describe your synthesis strategy.

For our first hypersketch, we will call Sketch's CEGIS Solver on our example sketch.

### Sketch and Data Files:

In order to demonstrate the generic nature of programs in HyperSketch,
we will have two running examples of sketches:

- synthesis of boolean expressions over bit arrays.
    - bitarray_synth.sk
    - bits.data
- synthesis of integer expressions over integer arrays.
    - intarray_synth.sk
    - ints.data

bitarray_synth.sk:
```
bit op(bit x0, bit x1) {
    return {| !(n0 && x1) | !(x0 || x1) | }
}
@FromFile("bits.data")
harness void sketch_main(int n, bit[n] bits, bit out) {
    assert(op(bits[??], bits[??]) == out);
}
```

bits.data:
```
2 {0, 0} 1
2 {0, 1} 0
2 {1, 0} 0
2 {1, 1} 0
```

intarray_synth.sk:
```
bit op(int x0, int x1) {
    return {| x0 + x1 | x0 - x1 | }
}
@FromFile("ints.data")
harness void sketch_main(int n, int[n] ints, int out) {
    assert(op(ints[??], ints[??]) == out);
}
```

ints.data
```
3 { 0, 1, 2 } 3
4 { 0, 1, 2, 3 } 6
2 { 4, 8 } 12
```

Notice the .data file formatting specification:
*.data:
```
row
row
...
```
where each row has space-seperated values that have types corresponding to the input type signature of the harness that will use this file for concretization of constraints.

For the purpose of this tutorial, all we really need to know about the sketches above is that they have a harness called sketch_main, and that both of them receive an inputs file.
```
@FromFile("path-to-data.data")
harness sketch_main(...) { ... }
```

To start off with our adventures in implementing synthesis strategies, we start by invoking a call to Sktch's CEGIS Solver by calling
SATSolver(SketchFunction, File).

[//]: # (solve_sketch&#40;SketchFunction, File, Finder = SATFinder [or NumericalFinder or EnumerationFinder]&#41;.)

my-hypersketch.hsk:
```
hypersketch main() {
    print("Hello World!!!");
    file = File(file_name, sketch_main__Wrapper);
    solution = SATSolver(sketch_main__Wrapper, file);
    program = sketch_main__Wrapper.produce_executable(solution);
    return program;
}
```

Now we are ready to call Sketch. Run:

``
  make run-local-seq EXEC_ARGS="bitarray_synth.sk"
``

Sketch should quickly output in the terminal code similar to the following (the followign code was manually styled):

```
SKETCH version 1.7.6
Benchmark = bitarray_synth.sk

void op (bit x0, bit x1, ref bit _out) {
  _out = !(x0 && x1);
  return;
}
@FromFile("bits.data")
void sketch_main(int n, bit[n] bits, bit out) {
  bit _out_s2 = 0;
  op(bits[0], bits[0], _out_s2);
  assert (_out_s2 == out);
}
@FromFile("bits.data")
void sketch_main__Wrapper (int n, bit[n] bits, bit out) 
implements sketch_main__WrapperNospec {
  sketch_main(n, bits, out);
}
void sketch_main__WrapperNospec (int n, bit[n] bits, bit out) {}

[SKETCH] DONE
Total time = 189
```

Now inspect the hypersketch_console.out output file:

hypersketch_console.out:
```
"Hello World!!!" 
BENCH[HyperSketchState::eval()]: 0 (s) ~ 1629 (us)
```

Finally, inspect the fmtl_program_file.fmtl, which was returned to the sketch-frontend by the sketch-backend to describe how to construct the final program.

fmtl_program_file.fmtl:

```
op = declare("op", ["H__2"], {});
op__id18 = op.unit_clone("op__id18", {"H__2" : "H__2__id18"});
op__id18.inplace_unit_concretize({"H__2__id18" : "0"});
sketch_main = declare(
  "sketch_main", ["H__0", "H__1"], {"op" : "op"});
sketch_main__id19 = sketch_main.unit_clone("sketch_main__id19",
  {"H__0" : "H__0__id19", "H__1" : "H__1__id19"});
sketch_main__id19.replace("op", op__id18);
sketch_main__id19.inplace_unit_concretize(
  {"H__0__id19" : "0", "H__1__id19" : "0"});
sketch_main__Wrapper = declare(
  "sketch_main__Wrapper", [], 
  {"sketch_main" : "sketch_main"});
sketch_main__Wrapper__id20 = sketch_main__Wrapper.unit_clone(
  "sketch_main__Wrapper__id20", {});
sketch_main__Wrapper__id20.replace(
  "sketch_main", sketch_main__id19);
sketch_main__Wrapper__id20.inplace_unit_concretize({});
return sketch_main__Wrapper__id20;
```

With that you have gone though the entire process of running hypersketch: passing a sketch file and .data file, passing the hypersketch file, and then inspecting the solution and output of the hypersketch program.


## Built-In Types, Methods, and Variables

1. built-in types
2. built-in methods
3. built-in variables

#### 1. Built-In Types

[//]: # (```)

[//]: # (Types:)

[//]: # (  void, namespace, Method)

[//]: # (  bool, int, float, string)

[//]: # (  SketchFunction, File, Solution, Input)

[//]: # (  PolyPair, PolyVec, PolyMap, PolyFrontier)

[//]: # (  )
```
Types:
  void, namespace, Method
  bool, int, float, string
  pair, vector, map, Frontier
  SketchFunction, File, Solution, Input
```

[//]: # (  syntax:)

[//]: # (    type name; // var-val-type-id : VarValType)

[//]: # (  )
[//]: # (  void _void; // void_val_type)

[//]: # (  int i; // int_val_type)

[//]: # (  bool b; // int_val_type)

[//]: # (  string s; // string_val_type)

[//]: # ( )
[//]: # (  float float_val; // float_val_type)

[//]: # (  File* file; // file_val_type)

[//]: # (  )
[//]: # (  Method* method; // method_val_type)

[//]: # (  SketchFunction* skfunc; // skfunc_val_type)

[//]: # (  HoleVarStore* solution; // solution_val_type)

[//]: # (  InputVarStore* input_holder; // input_val_type)

[//]: # (  )
[//]: # (  PolyVec* poly_vec; // poly_vec_type)

[//]: # (  PolyPair* poly_pair; // poly_pair_type)

[//]: # (  PolyMap* poly_map; // poly_map_type)

[//]: # (  PolyFrontier* poly_frontier; // poly_frontier_type)

[//]: # (```)

#### 2. Built-In Methods

```
  (method-name : str, method-id : MethodID, class-name : str)
  ("Frontier", _Frontier, "namespace");
  ("File", _file, "namespace");
  ("SATSolver", _sat_solver, "namespace");
  ("BatchEnumerationSolver", _batch_evaluation_solver, "namespace");
  ("print", _print, "namespace");
  ("timestamp", _timestamp, "namespace");
  ("float", _to_float, "namespace");
  ("assert", _assert, "namespace");
  ("not", _not, "namespace");
```

#### 3. Built-In Variables
    
    (t : Type, var-name : str, val-cpp : t);
    (bool, "true", true);
    (bool, "false", false);
    (int, "seed", seed);
    (string, "file_name", file_name);
    for(auto it: sketch_function_map){
          ("SketchFunction", it.first, function_map[it.first]);
    }

#### Note on Available Sketch Functions

Additionally, all the uninlined sketch functions (so harnesses and pure functions, but not generators, because generators get inlined in the frontend) from the sketch program (from the .sk file) will be available as variables in the namespace to be used as part of the hypersketch runtime (that's the whole point).
There are few more such wrapper functions which are implicitly instantiated as described in the next note on __Wrapper functions.

#### Note on __Wrapper Sketch Functions

As of current state of development, 
there is an implicit wrapper function for every harness. 
Each wrapper function's name is the constructed by concatinating with a '__Wrapper' suffix to the original harness name 
(e.g. if .sk file has [[ harness void sketch_function(...) ]], 
  then sketch_function__Wrapper will be the root function of the program space, 
  and it's sole purpose is to call sketch_function). 
This is due to historic reasons, 
and in future releases this will not be the case.  

