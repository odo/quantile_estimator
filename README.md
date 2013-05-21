# Quantile estimator written in Erlang

This library provides a memory-efficient data structures to store distributions of larges sets of data.

## Source

This implementation is based on:

Cormode et. al.:
"Effective Computation of Biased Quantiles over Data Streams"

Abstract:

"Skew is prevalent in many data sources such as IP traffic streams. To continually summarize the distribution of such data, a high-biased set of quantiles (e.g., 50th, 90th and 99th percentiles) with finer error guarantees at higher ranks (e.g., errors of 5, 1 and 0.1 percent, respectively) is more useful than uniformly distributed quantiles (e.g., 25th, 50th and 75th percentiles) with uniform error guarantees. In this paper, we address the following two problems. First, can we compute quantiles with finer error guarantees for the higher ranks of the data distribution effectively, using less space and computation time than computing all quantiles uniformly at the finest error? Second, if specific quantiles and their error bounds are requested a priori, can the necessary space usage and computation time be reduced? We answer both questions in the affirmative by formalizing them as the “high-biased ” and the “targeted ” quantiles problems, respectively, and presenting algorithms with provable guarantees, that perform significantly better than previously known solutions for these problems. We implemented our algorithms in the Gigascope data stream management system, and evaluated alternate approaches for maintaining the relevant summary structures. Our experimental results on real and synthetic IP data streams complement our theoretical analyses, and highlight the importance of lightweight, non-blocking implementations when maintaining summary structures over high-speed data streams."

## Installation

quantile_estimator requires rebar: https://github.com/basho/rebar

Building:
```
git clone git://github.com/odo/quantile_estimator.git
cd quantile_estimator/
make
```

## Usage

```erlang
plattfisch:quantile_estimator odo$ make shell
erl -pz ebin deps/*/ebin
Erlang R16A (erts-5.10) [source] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10  (abort with ^G)
1> IV = quantile_estimator:f_targeted([{0.05, 0.005}, {0.5, 0.02}, {0.95, 0.005}]).
#Fun<quantile_estimator.1.12316819>
```
So first we generated a function specifying that we want a guarantee that the 5th, 50th and 95th percentile are preserved with an error no more than 0.5%, 2% and 0.5 %, respectively. Please note the errors specified are errors in rank, the error in value might vary according to the shape of your distribution.

Now we take a new empty estimator and fill it with 10 000 random samples, giving a uniform distribution.

```erlang
2> QE = quantile_estimator:new(IV).
{quantile_estimator,0,0,0,[], #Fun<quantile_estimator.1.12316819>}
3> QERandom = lists:foldl(
    fun(_, QE) -> quantile_estimator:insert(random:uniform(), QE) end
    , QE
    , lists:seq(1, 10000)).
[...]
4> {Q005, Q05, Q095} = {
    quantile_estimator:quantile(0.05, QERandom)
    , quantile_estimator:quantile(0.5, QERandom)
    , quantile_estimator:quantile(0.95, QERandom)
}.
{0.04771803620573256,0.4810684987033724,0.9473718796803361}
```

Inserting and compressing are O(n) so it is a good idea to compress from time to time.
When the data is uncompressed as in the example, with successive compression passes the size will quickly converge to the minimum (here 1/120 of the original size after 11 passes).

```erlang
5> erts_debug:size(QERandom).
110066
6> {Sizes, QERandomCompressed} = lists:foldl(
    fun(_, {Sizes, QE}) -> QEC = quantile_estimator:compress(QE), {[{erts_debug:size(QEC)}|Sizes], QEC} end
    , {[], QERandom}
    , lists:seq(1, 20)
).
{[{902},
  {902},
  {902},
  {902},
  {902},
  {902},
  {902},
  {902},
  {902},
  {902},
  {913},
  {946},
  {1089},
  {1430},
  {2222},
  {3861},
  {7216},
  {14036},
  {27698},
  {55121}],
 {quantile_estimator,10000,76,0,
                     [{group,2.4128016570301725e-4,1,0,1},
                      {group,0.007869355524596777,67,20,2},
                      {group,0.012289777300297455,48,8,69},
                      {group,0.02016443337909468,68,19,117},
                      {group,0.027114225047992102,64,28,185},
                      {group,0.0324600651775544,56,33,249},
                      {group,0.0351798560323231,32,61,305},
                      {group,0.04220816673938166,68,17,337},
                      {group,0.048274226780547824,80,0,405},
                      {group,0.05430006206638027,62,0,485},
                      {group,0.06088946003756135,64,33,547},
                      {group,0.06343376254881306,32,54,611},
                      {group,0.06661410852498184,32,68,643},
                      {group,0.07409963150685184,79,38,675},
                      {group,0.07811247195447457,52,98,754},
                      {group,0.08519015363469817,80,27,806},
                      {group,0.09009917743705032,64,101,886},
                      {group,0.09858046185837788,90,80,...},
                      {group,0.11388492012352502,145,...},
                      {group,0.12492433355564558,...},
                      {group,...},
                      {...}|...],
                     #Fun<quantile_estimator.1.12316819>}}
7> {Q005C, Q05C, Q095C} = {
    quantile_estimator:quantile(0.05, QERandomCompressed)
    , quantile_estimator:quantile(0.5, QERandomCompressed)
    , quantile_estimator:quantile(0.95, QERandomCompressed)
}.
{0.05430006206638027,0.49830876273384694,0.9514831018482353}
```

We can confirm that the maximum error we are expecting is probably not exceeded.

```erlang
8> {abs(Q005 - Q005C), abs(Q05 - Q05C), abs(Q095 - Q095C)}.
{
  0.006582025860647711
  , 0.017240264030474517
  , 0.0041112221678991645
}
```

## Real world data

The test data set is the population of 14593 settlements in the US which is a Pareto distribution ("long tail").

# Performance

When inserting the test data (14593 numbers) and compressing every 10 inserts the rate of inserts is around 41 000 inserts/s.

# Compression rate

When compressions every 10 inserts, the size of the data structure converges to around 60 elements / bins.

![compression rate](https://raw.github.com/odo/quantile_estimator/master/doc/compression.png "compression rate")

# Accuracy

Values can be represented with high accuracy while maintaining high compression rates.

![accuracy](https://raw.github.com/odo/quantile_estimator/master/doc/accuracy.png "accuracy")

## Tests:

eunit:
```
make test
```
dialyzer:
```
make check
```
