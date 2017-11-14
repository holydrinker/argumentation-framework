## Argumentation Framework
#### Abstract Argumentation Solving Integrity Constraints

Prolog version used: YAP 6.2.2

#### Project structure
* `family.dat` is the dataset on which we made our experiments. The dataset represents a family tree. The file contains a predicate `examples(P, N, G, R)`. The predicate's arguments represent rispectively all positive examples, some negative ones, the information about the gender of every person, some facts about relationships (A married B, C is the parent of D, ...).

* `/kb-construction`: this folder contains the files that work con KB construction. The goal is start from a dataset.dat like `family.dat` and build up a `data.kb` file that store the information about the single dataset in a multi-head clause format, i.e.:
[head_1, head_2, ..., head_n] :- body_1, body2, ..., body_m.
This folder contains also a module that deal with dataset corrupution. If you are working only with positive examples you can corrupt the examples for generate new ambiguous facts and make more intresting the experiments.

* `/knowledge-base`: it's where the output of the `kb-construcition`'s process is stored.

* `/graph-construction`: This folder contains files that deal with the graph construction process starting from a knowledge base (possibly corrupted). This phase is based on a set of ad-hoc constraint (stored in the  `/graph-construction/constraints` folder), that explain what kinds of fact can coexist in the knowledge base. Based on these constraints, a set of labeled edge (attack edge or support edge) is generated, and the graph is built.

* `/graph`: it's where the output of the `graph-construcition`'s process is stored.
