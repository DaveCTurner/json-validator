# json-validator

Sample results of `stack build --file-watch --exec 'json-validator-exe --regress allocated:iters +RTS -T'`:

    benchmarking aeson/testEvent
    time                 30.31 μs   (29.84 μs .. 30.92 μs)
                         0.995 R²   (0.991 R² .. 0.998 R²)
    mean                 30.65 μs   (29.98 μs .. 31.53 μs)
    std dev              2.441 μs   (2.011 μs .. 3.097 μs)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              82970.577  (82951.640 .. 82990.374)
      y                  -2574.850  (-46423.830 .. 42563.513)
    variance introduced by outliers: 77% (severely inflated)

    benchmarking json-validator/Automaton/testEvent
    time                 20.47 μs   (20.05 μs .. 20.85 μs)
                         0.996 R²   (0.994 R² .. 0.998 R²)
    mean                 20.63 μs   (20.20 μs .. 21.12 μs)
    std dev              1.448 μs   (1.193 μs .. 1.912 μs)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              22450.061  (22436.588 .. 22463.399)
      y                  -3134.390  (-44552.047 .. 32262.181)
    variance introduced by outliers: 73% (severely inflated)


![Transitions Diagram](transitions.png)
