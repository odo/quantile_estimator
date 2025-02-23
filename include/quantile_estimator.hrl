-record(group, {
    v :: number(),
    g :: number(),
    delta :: number(),
    rank :: number()
}).
-record(quantile_estimator, {
    samples_count :: non_neg_integer(),
    data_count :: non_neg_integer(),
    inserts_since_compression :: non_neg_integer(),
    data :: [#group{}],
    invariant :: quantile_estimator:invariant()
}).
