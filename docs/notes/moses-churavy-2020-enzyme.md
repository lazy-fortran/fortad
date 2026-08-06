# Moses and Churavy 2020: Enzyme

Moses and Churavy present Enzyme as an LLVM compiler plug-in that synthesizes
gradients from statically analyzable LLVM IR. The motivation is foreign code:
scientific and systems programs should be differentiable without being
rewritten in a machine-learning DSL or using overloaded numeric types. Enzyme
targets C and C++ along with other languages that have an LLVM backend,
including Fortran and Julia.

## Design

The central pipeline has three stages. Type analysis reconstructs the types of
values in low-level memory. Activity analysis finds instructions that can
propagate a differential value. Synthesis emits an augmented forward pass and
a reverse pass that visits basic blocks and instructions in reverse order.

LLVM pointer types are too weak to determine the reverse operation for generic
memory instructions. Enzyme therefore builds type trees indexed by byte offset
and propagates them interprocedurally to a fixed point. TBAA metadata initializes
the facts for memory operations such as loads, stores, and `memcpy`. The paper's
example shows why an
8-byte copy needs different reverse code when it contains one `double` or two
`float` values.

Activity analysis uses alias analysis and type analysis to omit instructions
that cannot propagate an active value. Enzyme represents derivatives in shadow
allocations. It duplicates active arguments and data structures, delays
deallocation until the reverse pass is finished, and propagates shadows through
memory operations and calls.

Some reverse rules need forward values. Enzyme caches values that cannot be
recomputed and otherwise tries to recompute them. It uses a cost model and
differential-use analysis to reduce the tape. Alias information and cache reuse
also affect the decision. When legal, it combines the forward and reverse work
in one function.
Custom augmented-forward and gradient functions handle calls whose definitions
are unavailable. Multi-file use relies on link-time optimization so all needed
IR remains available.

## Optimization boundary and evidence

The paper's main performance claim is that AD should run after optimization.
Its normalization example becomes quadratic when differentiation happens before
loop-invariant code motion and linear when code motion happens first. In the
reported ADBench and additional tests, the optimized-before-AD pipeline was
4.5 times faster geometrically than the reference pipeline that differentiates
before the first optimization pass. The study used a quiesced AWS c4.8xlarge,
geometric means across 92 ADBench inputs plus separate integrator, FFT, and
Brusselator tests.

Enzyme needs IR for every function it must differentiate and enough metadata to
analyze memory types. It also needs
support for the relevant instruction set. The paper identifies missing
exception handling and runtime-created code as limitations. It also describes
embedding Enzyme in PyTorch, TensorFlow, and Julia to combine foreign-code
support with higher-level frameworks.

## Consequences for fortad

Enzyme is the direct performance reference for fortad. Fortad keeps more
Fortran type and control-flow information than LLVM IR. It should still run
semantics-preserving structural optimization before derivative emission when
that improves the derivative. The `norm` example is a warning that optimizing
generated code alone can miss the decisive transformation.

Fortad also needs explicit type and activity facts. Source-level types make
many memory cases easier than they are in LLVM, but aliasing and pointers still
need a conservative analysis. Calls and hidden state do too. A cache policy
should be visible in generated-code evidence, with recomputation, storage, and
peak memory reported alongside runtime. Enzyme's custom-rule interface is a
useful model for foreign or opaque procedures, while its LLVM assumptions mark
the boundary
where fortad should preserve source semantics instead of lowering early.

## Source

[NeurIPS 2020 paper](https://papers.neurips.cc/paper_files/paper/2020/file/9332c513ef44b682e9347822c2e457ac-Paper.pdf)

[arXiv:2010.01709](https://arxiv.org/abs/2010.01709)
