Urpal
=====

Urpal is your pal for Uppaal.

Urpal takes an Uppaal model as input, performs manipulations as described by a simple command language, and writes the resulting model back to a file. Its prime feature is the ability to construct a testing automaton for determining trace inclusion for a restricted class of timed automata. Uppaal can also: duplicate automata; create automata ready to accept all synchronisations on a set of channels; prune transitions based on their action label; conflate, drop and rename locations; and scale the graphical layout of a model. More details are in the manual page.

**See the main [website](http://www.tbrk.org/software/urpal.html)**

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

