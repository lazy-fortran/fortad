# LSTM rank-gap evidence

The Enzyme size-sweep artifact `20260809T043506Z-608942` recorded 18 explicit
engine/size gaps.  Fifteen were the LSTM intersection rows: FortAD's reverse
and gradient-only products failed Flang semantic checking at `fad_s15` and
`fad_s18`, where scalar temporaries received rank-one expressions.  The other
three rows were the unrelated Brusselator `N=1,000,000` process failure.

The failure was in reverse temporary shape inference.  Every `FAD_INDEX` was
treated as array-valued, including an element such as `z(i)`.  The IR now
marks only lowered range sections as array sections, so scalar indexed
temporaries remain scalar while genuine sections retain deferred shape.

The compiled numerical oracle is
[`test_lstm_oracle.f90`](../../test/test_lstm_oracle.f90).  It compiles and
runs both the with-primal VJP and gradient-only products, checks their
agreement, and checks a directional central-difference convergence step.

The reproducible five-size benchmark is
[`bench_lstm_rank_gap.sh`](../../scripts/bench_lstm_rank_gap.sh):

```text
scripts/bench_lstm_rank_gap.sh
```

Before this change, the independent sweep had 15 LSTM gaps at every size
(`100, 1,000, 10,000, 100,000, 1,000,000`) and neither derivative product
compiled.  After this change, both products compile for all five sizes, so
the LSTM gap count is 0/15; the unrelated suite gap remains 3/18.

One run on an AMD Ryzen 9 5950X with Flang 22.1.8 reported 10,502 generated
source bytes and 8,280 derivative-object bytes. Each row below is the median
of seven trials; repetitions keep the total work near one million elements.

| mode | N=100 | N=1,000 | N=10,000 | N=100,000 | N=1,000,000 |
|---|---:|---:|---:|---:|---:|
| VJP seconds | 0.121891 | 0.111002 | 0.088445 | 0.092270 | 0.081626 |
| gradient-only seconds | 0.111974 | 0.101922 | 0.084487 | 0.087257 | 0.078950 |
