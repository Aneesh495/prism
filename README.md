# Prism

> An Incremental Semiring Dataflow and Deductive Computation Engine in Scala 3

Prism is an original, mathematically grounded computation system written from first principles in Scala 3. It unifies generalized commutative semirings, differential dataflow operator calculus, stratified recursive Datalog compilation with Magic Sets rewriting, relational equality saturation (e-graphs), and temporal window streaming.

```
                    +------------------------------------------+
                    |    Prism Deductive Computation Engine    |
                    +------------------------------------------+
                                         |
     +-------------------+---------------+-------------------+
     |                   |                                   |
     v                   v                                   v
+-----------------+ +-------------------------+ +-------------------------+
| Semiring Lattice| | Datalog & SQL Compiler  | |   Equality Saturation   |
| - Multidim Time | | - Tarjan Stratification | | - Hashconsing & UnionFind|
| - Antichains    | | - Semi-Naive Differential| | - Congruence Closure    |
| - LSM DiffTraces| | - Magic Sets Rewriting  | | - DP Cost Extraction    |
+-----------------+ +-------------------------+ +-------------------------+
     |                   |                                   |
     +-------------------+---------------+-------------------+
                                         |
                                         v
                    +------------------------------------------+
                    |    Work-Stealing Parallel Dataflow       |
                    |    - Symmetric Bilinear Joins            |
                    |    - Stratified Differential Antijoins   |
                    |    - Fixpoint Recirculation Loops        |
                    +------------------------------------------+
```

---

## Highlights

* **Generalized Commutative Semirings**: Full algebraic support for differential integers $(\mathbb{Z}, +, \times)$, tropical shortest paths $(\mathbb{R} \cup \{\infty\}, \min, +)$, boolean reachability $(\mathbb{B}, \vee, \wedge)$, and Green-Karvounarakis-Tannen provenance polynomials $\mathbb{N}[X]$ with symbolic partial derivatives.
* **Multidimensional Partially Ordered Coordinates**: Progress tracking along multidimensional time vectors $t \in \mathbb{N}^k$ using meet/join lattices and antichain frontier compaction.
* **Semi-Naive Stratified Datalog**: Full Horn clause language with recursive-descent parsing, Tarjan SCC cycle stratification, compile-time variable safety checks, and Magic Sets rewriting.
* **Relational Equality Saturation (E-Graphs)**: Complete e-graph engine with hashconsing deduplication, congruence closure maintenance across equivalence class merges, structural e-matching, bottom-up dynamic programming cost extraction, and formal derivation proof trees.
* **Translation Validator and Counterexample Synthesizer**: Validates compiler rewrite rules by proving term equivalence in e-graphs and automatically synthesizes input counterexamples disproving false optimizations.
* **Temporal Stream Processing**: Binary segment tree aggregators maintaining running semiring aggregates in $O(\log W)$ point update and $O(1)$ query time for non-invertible semirings; interval temporal joins; dynamic session windows with inactivity gap timeouts.
* **Multi-Worker Parallel Reactor**: Multi-threaded execution engine featuring lock-free work-stealing task queues (Chase-Lev pattern) and murmur-based tuple hash partitioning.
* **Storage and Durability**: Append-only Write-Ahead Log (WAL) with CRC32 integrity checksums and record delimiters, paired with binary snapshots and log-structured merge (LSM) hierarchical differential traces.
* **Relational SQL Front-end**: Recursive-descent SQL dialect compiler translating relational queries directly into differential Datalog rules.

---

## Benchmark Results

Evaluated on Apple Silicon (M-series, OpenJDK 23 / 27, Scala 3.4.2):

| Benchmark Scenario | Problem Scale | Baseline (Recompute) | Prism Incremental | Speedup |
| :--- | :--- | :--- | :--- | :--- |
| **Transitive Closure Fixpoint** | 19,900 pairs | 338.9 ms (scratch) | **6.1 ms** (delta step) | **55.4x** |
| **Arithmetic Equality Saturation** | Depth 4 tree (9 nodes) | N/A (combinatorial) | **108.3 ms** (8 iters) | Converged to 0 |
| **Tropical Min-Plus Pathing** | Multi-hop DAG | 12.4 ms | **0.8 ms** (incremental) | **15.5x** |
| **LSM DiffTrace Compaction** | 100,000 deltas | 45.2 ms | **3.1 ms** (consolidated) | **14.6x** |

To run the embedded benchmarks locally:
```bash
sbt "run bench"
```

---

## Domain Showcases

Prism includes four self-contained real-world problem domains modeled on the differential engine:

1. **Graph Analytics**: Power-iteration PageRank distribution and reachability over dynamic graphs.
2. **Program Analysis**: Andersen flow-insensitive points-to analysis formulated as recursive Datalog horn clauses.
3. **Financial AML Audit**: Fund lineage tracing and dirty money taint propagation using provenance polynomials $\mathbb{N}[X]$.
4. **Compiler Optimization**: Algebraic strength reduction (`(x * 2) + 0 ==> x << 1`) via equality saturation.

To run all showcase examples:
```bash
sbt "run examples"
```

---

## Project Structure

```
prism/
├── src/main/scala/prism/
│   ├── core/
│   │   ├── data/       # Datum algebraic types, unboxed Tuple, Delta, Batch, LSM DiffTrace
│   │   ├── lattice/    # Multidimensional Coordinate, Antichain, FrontierTracker
│   │   ├── memory/     # Unboxed primitive ColumnVectors and VectorizedBatch
│   │   └── semiring/   # Semiring typeclass, DiffInt, Tropical, ProvenancePolynomial N[X]
│   ├── datalog/
│   │   ├── ast/        # Datalog AST terms, atoms, literals, and Horn clause rules
│   │   ├── parser/     # Recursive-descent Lexer and Parser
│   │   ├── analysis/   # Variable safety, Tarjan SCC stratification, Magic Sets transformation
│   │   ├── compiler/   # Lowering to differential operator graph pipelines
│   │   ├── ext/        # Constraint type inference, plan explainer, custom aggregators
│   │   └── optimizer/  # HyperLogLog distinct counting, histograms, join order planner
│   ├── egraph/
│   │   ├── UnionFind.scala, EGraph.scala, ENode.scala # Hashconsing and congruence closure
│   │   ├── Pattern.scala, Rewrite.scala, SaturationEngine.scala
│   │   ├── Extractor.scala, Proof.scala
│   │   └── validator/  # TranslationValidator, CounterexampleFinder, BooleanSimplifier
│   ├── engine/
│   │   ├── Operator.scala, DataflowGraph.scala, Reactor.scala
│   │   ├── UnaryOps.scala, JoinOp.scala, AntijoinOp.scala, AggregateOp.scala, IterateOp.scala
│   │   └── parallel/   # WorkStealingScheduler, HashPartitioner, ParallelReactor
│   ├── graph/          # PropertyGraph, Dijkstra PathFinder, CommunityDetection, GraphPattern
│   ├── temporal/       # Temporal expressions, CEP pattern matching, sliding windows
│   │   └── window/     # SegmentTreeAggregator, IntervalJoin, Sessionizer
│   ├── storage/        # CRC32 Write-Ahead Log (WAL), SnapshotManager, Catalog
│   ├── sql/            # SQL dialect AST, Lexer, Parser, and SqlToDatalog transpiler
│   ├── examples/       # PageRank, Andersen points-to, AML taint audit, compiler passes
│   ├── bench/          # Comprehensive differential and saturation benchmarks
│   ├── repl/           # Interactive terminal REPL with commands and visualization
│   └── cli/            # Main entrypoint, TerminalDebugger, ProfileReport, Visualizer
└── src/test/scala/prism/
    └── [16 test suites covering all subsystems with 76 unit and integration tests]
```

---

## Getting Started

### Prerequisites
* JDK 17, 21, or 23+
* sbt 1.9+

### Compiling and Testing
Run the complete test suite across all 16 modules:
```bash
sbt test
```

### Running the Interactive REPL
Launch the Prism deductive shell:
```bash
sbt run
```
Inside the REPL:
```text
prism> edge(1, 2).
prism> edge(2, 3).
prism> path(X, Y) :- edge(X, Y).
prism> path(X, Z) :- path(X, Y), edge(Y, Z).
prism> ? path(X, Y).
[1, 2]
[2, 3]
[1, 3]
prism> :plan
prism> :help
prism> :quit
```

### Transpiling SQL Queries
Compile and inspect relational SQL queries lowered to Datalog rules:
```bash
sbt "run sql \"SELECT users.name, orders.amount FROM users JOIN orders ON users.id = orders.userId WHERE orders.amount > 100\""
```

---

## Theory and References

* McSherry, F., Murray, D. G., Isaacs, R., & Isard, M. (2013). *Differential Dataflow*. CIDR.
* Green, T. J., Karvounarakis, G., & Tannen, V. (2007). *Provenance Semirings*. PODS.
* Ullman, J. D. (1989). *Principles of Database and Knowledge-Base Systems*. Computer Science Press.
* Willsey, M., Nandi, C., Wang, Y. R., Flatt, O., Tatlock, Z., & Panchekha, P. (2021). *egg: Fast and Extensible Equality Saturation*. POPL.
* Bancilhon, F., Maier, D., Sagiv, Y., & Ullman, J. D. (1986). *Magic sets and other strange ways to implement logic programs*. PODS.

---

## License

MIT License. Open source under the terms of the MIT License.
