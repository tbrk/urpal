Urpal
=====

Urpal is your pal for Uppaal.

Urpal takes an Uppaal model as input, performs manipulations as described by a simple command language, and writes the resulting model back to a file. Its prime feature is the ability to construct a testing automaton for determining trace inclusion for a restricted class of timed automata. Uppaal can also: duplicate automata; create automata ready to accept all synchronisations on a set of channels; prune transitions based on their action label; conflate, drop and rename locations; and scale the graphical layout of a model. More details are in the manual page.

**[main website](http://www.tbrk.org/software/urpal.html)**

Descriptions of the testing construction can be found in:

* The thesis of Mariëlle Stoelinga, specifically Chapter 7 and Appendix A. Development on Urpal began from her descriptions at the suggestion of Frits Vaandrager.

* The _Scaling up Uppaal_ paper of Henrik Ejersbo Jensen, Kim Guldstrand Larsen, and Arne Skou. Urpal incorporates their construction for handling urgent locations and shared variables.

* The paper by Arcot Sowmya and me, to be presented at EMSOFT 2008. This paper presents several improvements to the original CSE technical report (UNSE-CSE-TR-0723).

The testing construction cannot be performed on all timed automata,
specifically several modelling features are not supported:

* non-deterministic automata (this assumption is not checked),
* non-synchronizing (τ) transitions,
* committed locations,
* process priorities,
* channel priorities,
* inputs on broadcast channels,
* some combinations for forall and select bindings (see this paper).

The software was developed at the The University of NSW and NICTA. It is released under a BSD License. Urpal uses technology developed with the imagination and ideas from NICTA, Australia's ICT Centre of Excellence. The source repository can be browsed online.

This software is provided as a prototype only: use at your own risk! It surely contains faults, please report them.

Graphviz is required to run Urpal. The source code has additional compilation dependencies.

Building Urpal
--------------

### Software required for Compilation

Urpal relies on several external libraries:

* The [Standard ML Basis Library](http://www.standardml.org/Basis/).

* The [Fxp](http://www2.informatik.tu-muenchen.de/~berlea/Fxp/) XML parser.

* Parts of the [SML/NJ libraries](http://www.smlnj.org/doc/smlnj-lib/index.html) as distributed with SML/NJ and MLton, and readily [adapted for Poly/ML](http://www.tbrk.org/software/poly_smlnj-lib.html), including [ML-Yacc](http://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/) and [ML-Lex](http://www.cs.princeton.edu/~appel/modern/ml/ml-lex/).

A compiler or interpreter is also required. Urpal was developed with [SML/NJ](http://www.smlnj.org/), but [MLton](http://mlton.org/) is used to compile the [binaries](http:/www.tbrk.org/software/urpal.html#download) (cross-compiling with [mingw](http://www.mingw.org/) for windows). [Poly/ML](http://www.polyml.org/) can also be used. Instructions for each of the three systems follow.

### Compilation

Compilation interdependencies are specified over various files:
* `*.cm`: for SML/NJ
* `*.mlb`: for MLton
* `*.ml`: `use` commands for Poly/ML

Compilation has only been tested on Linux and FreeBSD, but I am happy to help getting it to work under Windows.

Both [ML-Yacc](http://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/) and [ML-Lex](http://www.cs.princeton.edu/~appel/modern/ml/ml-lex/) are normally required to turn the `*.grm` and `*.lex` files into SML code, but the source distribution includes files generated with SML/NJ that also work with Poly/ML. Otherwise, the relevant files can be produced with: `make lexfiles grmfiles`.

#### SML/NJ

SML/NJ must be provided with the path to the Fxp sources. One possibility is 
add a line to `$HOME/.smlnj-pathconfig`:
```
fxlib.cm	    /usr/lib/sml-fxp/src
```

To build an executable: `cd src; make`.

To load into an interactive session:
```
cd src;
sml
CM.make "sources.cm";
```

Only the structures [Urpal](src/urpal.sml) and [Settings](src/settings.sml), and the signature [SETTINGS](src/settings.sig) are imported.

#### MLton

* Edit [src/unix-path-map](src/unix-path-map), setting the `SML_FXP` path.
* `cd src`
* `make clean`
* `make withmlton`

_Note:_ Compiling first with SML/NJ and then with MLton may fail because the lexer produced by the former is not acceptable to the latter. Running `make clean` before compilation with MLton fixes the problem.


#### Poly/ML

~~Urpal _cannot_ currently be compiled in Poly/ML due to an [internal error](http://www.mail-archive.com/polyml@inf.ed.ac.uk/msg00026.html).~~

The ML-Yacc and SML/NJ libraries must be [adapted](http://www.tbrk.org/software/poly_smlnj-lib.html) for Poly/ML.

* `cd src`
* Edit the `root` variable in [src/fxlib.ml](src/fxlib.ml).
* Edit the `root` variable in [src/smlnj-lib.ml](src/smlnj-lib.ml).
* Edit the `root` variable in [src/mlyacc.ml](src/mlyacc.ml).
* Start Poly/ML (`poly`).
* Import Urpal: `use "urpal.ml";`

#### Other

The core SML language is standardised and, ignoring implementation flaws, always works as expected across different compilers and interpreters. There are, however, two major obstacles to portability. Several environments were available before the SML Basis Library was finalised, and thus the interfaces of ageing environments do not always comply with the [final standard](http://www.cambridge.org/us/catalogue/catalogue.asp?isbn=0521794781)</a>. More problematic is that some library implementations are incomplete, even omitting _required_ signatures and structures. The second obstacle is the existence of several systems for compiling multiple SML files into a single system. For these reasons, Urpal cannot be executed in all environments. Those explicitly not supported are:

* [SML.net](http://www.cl.cam.ac.uk/research/tsg/SMLNET/): lacks functional I/O (`TextIO.StreamIO`).
* [Moscow ML](http://www.dina.dk/~sestoft/mosml.html): lacks functional I/O (`TextIO.StreamIO`), requires filename changes.

Execution has not been attempted with [Alice](http://www.ps.uni-sb.de/alice/), [HaMLet](http://www.ps.uni-sb.de/hamlet/), [MLKit](http://www.it-c.dk/research/mlkit/index.php/Main_Page), or [SML#](http://www.pllab.riec.tohoku.ac.jp/smlsharp/). Comments or instructions would be gratefully [accepted](mailto:tim@tbrk.org).

Modules
-------

The program is built from several interdependent modules. There is a separate subdirectory in [src](src) for each, except [main](#module-main) whose files are stored at the root. The modules are listed below with brief descriptions and notes for possible improvement.

There is usually one source file per signature, structure, or functor. The files follow a simple naming pattern:

* `cmd_lang.sig` contains the definition of the `CMD_LANG` signature.
* `cmdlang.sml` contains the structure `CmdLang` which provides the primary implementation of `CMD_LANG`.
* `plainfn.sml` contains the functor `PlainFn`. All functor names end with `Fn`.

### Module: Main

The main structure that dispatches program options to subroutines is given in [urpal.sml](src/urpal.sml); the expression in [main.sml](src/main.sml) invokes the relevant function with arguments from the command line. Command line options are parsed by [commands.sml](src/commands.sml).
  
The various `settings` files parse and create [urpalrc](src/urpalrc) files, and store global program settings. Currently it is difficult to add new settings; this should be fixed.

The file [util.sml](src/util.sml) contains functions for printing debugging messages, warnings, and errors. It could be better named.

### Module: Cmdlang

Implements the expression language used to specify manipulations of Uppaal models. A lexical analyser [cmdlang/cmdlang.lex](src/cmdlang/cmdlang.lex) and parser [cmdlang/cmdlang.grm](src/cmdlang/cmdlang.grm) are driven by [cmdlang/cmdlang.sml](src/cmdlang/cmdlang.sml).

There are probably many other useful manipulations of Uppaal models that could be implemented.
  
This module is specific to Urpal.
  
### Module: Config

A generic module for parsing hierarchical configuration files, following the syntax of Reppy's [ML-Doc](http://people.cs.uchicago.edu/~jhr/tools/ml-doc.html).
  
### Module: Graphviz

Generic functions for interacting with [Graphviz](http://www.graphviz.org/). With enhancements, this module would make a good separate library.
  
Types and functions for dot files, including representation ([graphviz/dot.sig](src/graphviz/dot.sig), [graphviz/dotfn.sml](src/graphviz/dotfn.sml), [*_attribute.sig](src/graphviz)), and pretty-printing ([graphviz/dotppfn.sml](src/graphviz/dotppfn.sml)), but not parsing. There are some limitations: anonymous subgraphs cannot be used in edges (no `n1 -> subgraph {...}`), and there are no explicit types for cluster or subgraph attributes.
  
Types and functions for the plain format, including representation ([graphviz/plain.sig](src/graphviz/plain.sig)), pretty-printing, and parsing ([graphviz/plainfn.sml](src/graphviz/plainfn.sml)). General HTML labels (multiple tags) are not supported.
  
There is an interface for invoking Graphviz commands as subprocesses and parsing the results: [graphviz/graphviz.sig](src/graphviz/graphviz.sig), [graphviz/graphvizfn.sml](src/graphviz/graphvizfn.sml). The functor allows specialisation for Unix or Windows. Support for the latter is fudged because the Basis library Windows structure is not frequently implemented. More fudging is required to handle word size differences between MLton and SML/NJ.
  
A simple signature [graphviz/x11_color.sig](src/graphviz/x11_color.sig) and structure [graphviz/x11color.sml](src/graphviz/x11color.sml) for manipulating <a href="http://en.wikipedia.org/wiki/X11_color_names">X11 colour names</a>. These files would be in a separate module except that they are only used by the graphviz components.

The multiplication of [*-graphviz*.sml](src/graphviz/) and [*-signal*.sml](src/graphviz) structures is unfortunate, but required for compatibility across different environments. There is probably a better way to do this; if even to preprocess files.
  
### Module: Layout

The beginnings of a generic interface for visual formatting of Uppaal models. More work is needed to make this module generic and reusable. One good feature would be to reduce the number of nails introduced by Graphviz, particularly by omitting several that occur close together. 

### Module: Lib

Utility functions that do not fit anywhere else. Including a file position abstraction used in the lexers ([lib/file_pos.sig](src/lib/file_pos.sig), [lib/filepos.sml](src/lib/filepos.sml)) and some oft-used shortcuts over atoms ([lib/symbol.sig](src/lib/symbol.sig) and [lib/symbol.sml](src/lib/symbol.sml)).
  
### Module: Maketest

This module implements the testing construction described in this [paper](http:/www.tbrk.org/papers/abstracts.html#emsoft08).
  
It has improved greatly over time but another refactoring is probably required to:

* Handle/rename bindings more generically and automatically.

* Remove the redundant routines for the old ‛partitioning’ algorithm (see `negatePartitionedTransitions` in [maketest/transitionflipper.sml](src/maketest/transitionflipper.sml), see [UNSW-CSE-TR-0723](ftp://ftp.cse.unsw.edu.au/pub/doc/papers/UNSW/0723.pdf)). This has not been done before now in case further comparisons of the two approaches are required.

* Rationalise/tidy the functions that handle urgent channels.

* Make clock expressions and associated manipulations a separate and more generic unit, possibly placing them in the [uppaal](module-uppaal) module.
  
### Module: Mcs51

Simple-minded routines for parsing [MCS51](http://www.intel.com/design/mcs51/index.htm) (also known as 8051) assembler and creating rudimentary Uppaal models.
  
These functions exist more as an example of how the other modules can be combined for useful effect rather than as a proposal for a practical model-checking of assembly programs. The lexer, parser, and data structures may form a good starting point for other programs that manipulate MCS51 assembly code.
  
### Module: Uppaalxml

Types and routines for parsing, representing, and manipulating the Uppaal XML file format. These form, together with those in the [uppaal](#module-uppaal) module, an alternative in Standard ML to [libutap](http://www.cs.aau.dk/~behrmann/utap/). They could be useful for building other programs that manipulate Uppaal models.
  
The files [uppaalxml/nta.sig](src/uppaalxml/nta.sig), [uppaalxml/nta_output.sig](src/uppaalxml/nta_output.sig), [uppaalxml/nta_types.sig](src/uppaalxml/nta_types.sig), [uppaalxml/nta_types_output.sig](src/uppaalxml/nta_types_output.sig), [uppaalxml/ntafn.sml](src/uppaalxml/ntafn.sml), and [ntaoutputfn*.sml](src/uppaalxml/) express and manipulate the basic structure of an Uppaal NTA file. (Unfortunately [uppaalxml/ntaoutputfn.sml](src/uppaalxml/ntaoutputfn.sml) and [uppaalxml/ntaoutputfn.nj.sml](src/uppaalxml/ntaoutputfn.nj.sml) are duplicates but for argument constraints, to work around a flaw in SML/NJ's behaviour.) There are two concrete instantiations: one, [uppaalxml/textnta.sml](src/uppaalxml/textnta.sml), where expressions on transitions, functions, etcetera, are uninterpreted text, and another, [uppaal/parsednta.sml](src/uppaal/parsednta.sml), in the [uppaal](#module-uppaal) module in which they have been parsed into datatypes.
  
The other files interface with the [FXP](http://www2.informatik.tu-muenchen.de/~berlea/Fxp/) XML parsing library. The [flat-1_1.dtd](http://www.it.uu.se/research/group/darts/uppaal/flat-1_1.dtd) file, which the parser uses to validate its input, is currently read from a local path. Configuration would be simpler if it could be built into the parser, but this does not seem to be possible without modifying the FXP routines. No attempt is currently made to download this file automatically, although FXP supports such a feature.
  
The files [uppaalxml/xml_writer.sig](src/uppaalxml/xml_writer.sig) and [uppaalxml/xmlwriter.sml](src/uppaalxml/xmlwriter.sml) plug into the Basis library I/O routines for escaping certain characters (<, >, &, ", and ') when outputting XML. They are reusable of themselves.

### Module: Uppaal

Types and routines for parsing, representing, manipulating, and pretty printing the Uppaal command language. There are three main parts: expressions, declarations, and environments. Each has routines for pretty printing [*pp.sig/sml](src/uppaal) and conversion to strings [*cvt.sig/sml](src/uppaal)</a>.
  
The lexer [uppaal/uppaal.lex](src/uppaal/uppaal.lex) and parser [uppaal/uppaal.grm](src/uppaal/uppaal.grm) turn uninterpreted Uppaal expressions and commands into expression and declaration types. The files [uppaal/uppaal_parse.sig](src/uppaal/uppaal_parse.sig) and [uppaal/uppaalparse.sml](src/uppaal/uppaalparse.sml) perform some semantic analysis─but not full type checking─using an environment [uppaal/environment.sig](src/uppaal/environment.sig)/[uppaal/environment.sml](src/uppaal/environment.sml).
  
The expression structure has a `unresolvedty` mechanism for handling array declarations like `int A[T]` where `T` could be a typedef, like `typedef int[0,3] T`, or a constant parameter. The mechanism exists because parsing and semantic analysis are strictly separated. A more principled alternative would be to have two signature/structure pairs: one for parsed expressions, the other for analysed expressions. The shared declarations could be factored out into a functor.

The environment structure requires refactoring.

### Other ideas

* Integration with a theorem prover, such as [Isabelle](http://isabelle.in.tum.de/) to double-check or justify manipulations of automata and expressions.

* Exploit the term-rewriting module of [Isabelle](phttp://isabelle.in.tum.de/) or similar.

Step-by-step Example
--------------------

1. Download and extract Urpal.

2. Download the Uppaal DTD, flat-1_1.dtd.

3. Install Graphviz if necessary.

4. Edit urpalrc, setting the paths to flat-1_1.dtd and Graphviz:
	```
	   dtd_path="flat-1_1.dtd"
	   graphviz {
	       path = "/usr/local"
	       engine = neato
	   }
	```
Urpal will look for the Graphviz executables, like fdp and neato, in a bin subdirectory of the path given.

5. Download the simple [example model](http://www.tbrk.org/software/urpal-example.xml).

6. Start a terminal, and run urpal:
   `./urpal -i urpal-example.xml -o urpal-example-after.xml - e "Test=maketest(Template)"`
In the Windows command interpreter (cmd.exe), type rather:
   `urpal -i urpal-example.xml -o urpal-example-after.xml -e "Test=maketest(Template)"`

7. The program should produce a model urpal-example-after.xml that can be opened in Uppaal. The new model should contain the original Template automaton and a new Test automaton.

One good way to verify the result is to model check the original automaton against the generated test automaton, which also exposes models that are not deterministic. This would be done for the above example by first changing the system declaration to: `system Template, Test;`, then verifying the formula: `A[] (not Test.Err)`. Such tests have previously revealed faults in Urpal. Another test is to verify that no new deadlocks are introduced, as test automata are always ready to synchronize. Since there are already two possibilities for deadlock in the example, the formula is:
  `A[] (deadlock imply ((Template.s2 and Template.x >=3 and Template.x < 4) or Template.s3))`

The source repository includes a number of small test models, and a larger railway crossing controller example.

Improving the appearance of results
-----------------------------------

The layout routines rarely give perfect results, but they usually make the basic structure clear enough so that sense can be made of counter-example traces, and they are a reasonable starting point for manual rearrangement of label positions (the transition nails make other changes painful). If the results are very poor, try:

* Scaling the source model before generating the test automaton. For example, `Test=maketest(scale(Template, 2.0))`.

* Tabulating the labels on transitions to the error state. For example, `Test=tabulate(maketest(Template), {Err})`.

* Trying different Graphviz layout routines. For example, either fdp `set=graphviz{engine=fdp}` or the other spring-based approach neato `set=graphviz{engine=neato}`. Usually one will give better results than the other for a particular model.

* Various combinations of the above.

* Ultimately, manual rearranging the original automaton or the resulting test automaton is sometimes the only satisfactory approach. Scaling the model and using the tabulate feature can make it easier.

