# Architecture Specification: Prism Differential Dataflow Engine

Prism is an incremental deductive computation engine written in Scala 3 that unifies generalized semiring differential dataflow, stratified recursive Datalog evaluation with Magic Sets rewriting, and relational equality saturation (e-graphs).

```
                            +--------------------------+
                            |   SQL / Datalog / AST    |
                            +--------------------------+
                                         |
                                         v
                     +----------------------------------------+
                     |  Compiler: Stratification & Magic Sets |
                     +----------------------------------------+
                                         |
                                         v
                     +----------------------------------------+
                     |   Physical Differential Operator DAG   |
                     +----------------------------------------+
                                         |
        +--------------------------------+--------------------------------+
        |                                |                                |
        v                                v                                v
+---------------+                +---------------+                +---------------+
|  Bilinear Join|                | Stratified Not|                | Fixpoint Loop |
+---------------+                +---------------+                +---------------+
        \                                |                                /
         +-------------------------------+-------------------------------+
                                         |
                                         v
                     +----------------------------------------+
                     |  Reactor: Quiescence Scheduler & LSM   |
                     +----------------------------------------+
                                         |
                                         v
                     +----------------------------------------+
                     |    Lattice Frontiers & Semiring Trace  |
                     +----------------------------------------+
```

## 1. Mathematical Foundations

### 1.1 Multidimensional Lattice Timestamps and Antichain Frontiers
Computations in Prism advance along multidimensional partially ordered coordinate spaces. A timestamp $t \in \mathbb{N}^k$ represents progress across epochs and recursive loop iterations.
Given two coordinates $u, v \in \mathbb{N}^k$:
* $u \le v \iff \forall i \in \{0, \dots, k-1\}, u_i \le v_i$
* Meet operation: $(u \wedge v)_i = \min(u_i, v_i)$
* Join operation: $(u \vee v)_i = \max(u_i, v_i)$

An **Antichain Frontier** $\mathcal{F} \subset \mathbb{N}^k$ represents a set of mutually incomparable coordinates such that for any active computation at timestamp $t$, there exists some $f \in \mathcal{F}$ with $f \le t$. When a coordinate is no longer reachable by upstream operators, frontiers advance, enabling memory reclamation and compaction of historical traces.

### 1.2 Commutative Semiring Weights
Every relational tuple in Prism is annotated with a multiplicity weight $w \in \mathcal{R}$ belonging to a commutative semiring $(\mathcal{R}, \oplus, \otimes, 0, 1)$:
* **Differential Integer Abelian Group** $(\mathbb{Z}, +, \times, 0, 1)$: Tracks positive tuple insertions and negative tuple retractions.
* **Tropical Semiring** $(\mathbb{R} \cup \{\infty\}, \min, +, \infty, 0)$: Computes dynamic single-source shortest paths and optimal route finding.
* **Boolean Lattice** $(\mathbb{B}, \vee, \wedge, \text{false}, \text{true})$: Models classical boolean reachability and existence without multiset counts.
* **Provenance Polynomials** $\mathbb{N}[X]$: Green-Karvounarakis-Tannen provenance semiring tracking symbolic lineage, derivation paths, and algebraic sensitivity derivatives.

## 2. Differential Operator Calculus

Prism represents computations as directed acyclic dataflow topologies containing cyclic feedback loops for recursive fixpoints.

### 2.1 Symmetric Bilinear Differential Join
Given two relations $A$ and $B$, their cross product over time satisfies the differential Leibniz product rule:
$$\Delta(A \Join B) = (\Delta A \Join B) \oplus (A \Join \Delta B) \oplus (\Delta A \Join \Delta B)$$
In Prism, each join operator maintains indexed state traces for both inputs. When a batch $\Delta A$ arrives at timestamp $t$:
1. It probes the accumulated history of $B$ strictly prior to $t$.
2. It probes contemporaneous deltas of $B$ at timestamp $t$.
3. Resulting deltas are weighted by $w_{\Delta A} \otimes w_B$ and emitted downstream.

### 2.2 Stratified Differential Antijoin (Negation)
Stratified negation computes $A \setminus B$. In incremental dataflow:
* Insertion of key $k$ into $B$ triggers a retraction of matching keys in $A$.
* Retraction of key $k$ from $B$ triggers an assertion of matching keys in $A$.
Negation is constrained to stratified dependency graphs, preventing unstratifiable recursive paradoxes.

### 2.3 Recursive Loop Feedback (IterateOp)
Recursive strata (such as transitive closure reachability or pointer analysis) are evaluated via feedback operators. The `IterateOp` routes outputs back to its recirculating input port, incrementing the iteration dimension in coordinate space until no new non-zero deltas are generated (quiescence).

## 3. Datalog Engine and Compiler

The Datalog subsystem accepts formal Horn clauses, parses them via recursive descent, verifies variable safety, and stratifies dependency graphs:
1. **Safety Analysis**: Verifies that all head variables and variables inside negated literals appear in at least one positive body atom.
2. **Tarjan SCC Stratification**: Computes Strongly Connected Components across predicate dependency graphs. Edges representing negated literals or aggregations must strictly cross strata boundaries.
3. **Magic Sets Transformation**: Applies Adorned Rule Sets and Supplementary Magic Predicates to push query constants down into recursive relations, pruning unnecessary subcomputations.
4. **Physical Lowering**: Translates strata into differential operator pipelines (`FilterOp`, `MapOp`, `JoinOp`, `AntijoinOp`, `AggregateOp`, `IterateOp`).

## 4. Relational Equality Saturation (E-Graphs)

Prism contains a complete, verified Equality Saturation engine for term rewriting and formal translation validation:
* **E-Node Hashconsing**: Guarantees structural deduplication across terms.
* **Congruence Closure Maintenance**: When two e-classes are merged via Union-Find with path compression, upward parent pointers are traversed during `rebuild()` to enforce congruence closure.
* **EMatcher**: Structural pattern matching supporting variable wildcards.
* **Cost Extraction**: Bottom-up dynamic programming extracting minimal-cost expressions from equivalence graphs.
* **Proof Generation**: Constructs human-readable derivation certificates verifying why two expressions are mathematically identical.
* **Counterexample Synthesizer**: Randomly probes variable valuations to synthesize concrete counterexamples disproving invalid equivalence claims.

## 5. Storage and Fault Tolerance

* **Write-Ahead Log (WAL)**: Binary append-only log writing batch records with CRC32 integrity checksums and record delimiters.
* **Binary Snapshotting**: Encodes state snapshots to disk using high-speed serialization for instant recovery.
* **Hierarchical DiffTraces**: Log-structured merge (LSM) hierarchical indexes organizing deltas into levels to ensure $O(\log N)$ historical point queries and compaction.

## 6. Concurrency and Parallel Execution

The `ParallelReactor` coordinates multi-threaded differential evaluation:
* **Work-Stealing Task Scheduler**: Lock-free task deques allow worker threads to process active operator steps concurrently, with idle workers stealing tasks.
* **Hash Partitioning**: Murmur-based tuple hashing splits incoming batches across worker threads without cross-thread lock contention.
* **Synchronous Epoch Frontiers**: Output collection and frontier advancement remain deterministic across workers.

## 7. Temporal Processing and SQL Front-end

* **Segment Tree Sliding Aggregation**: Binary segment trees enable $O(\log W)$ point updates and $O(1)$ range queries for non-invertible semirings.
* **Interval Differential Join**: Joins streaming temporal events whose timestamps fall within $[t_r - \delta_1, t_r + \delta_2]$.
* **Session Window Aggregation**: Groups event streams by inactivity gap timeouts, dynamically bridging split sessions upon out-of-order event arrival.
* **Relational SQL Dialect**: Parses SQL queries (`SELECT`, `FROM`, `JOIN`, `WHERE`, `GROUP BY`) and lowers them directly into differential Datalog rules.
