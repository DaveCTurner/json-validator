# json-validator

Sample results of `stack build --file-watch --exec 'json-validator-exe --regress allocated:iters +RTS -T'`:

    benchmarking aeson/testEvent
    time                 41.55 μs   (40.43 μs .. 42.65 μs)
                         0.992 R²   (0.988 R² .. 0.996 R²)
    mean                 41.58 μs   (40.34 μs .. 43.09 μs)
    std dev              4.615 μs   (3.461 μs .. 6.448 μs)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              82972.201  (82936.205 .. 83008.598)
      y                  -3494.098  (-50283.941 .. 43634.160)
    variance introduced by outliers: 87% (severely inflated)

    benchmarking json-validator/Automaton/testEvent
    time                 31.93 μs   (31.44 μs .. 32.51 μs)
                         0.997 R²   (0.995 R² .. 0.998 R²)
    mean                 32.07 μs   (31.52 μs .. 32.75 μs)
    std dev              2.196 μs   (1.709 μs .. 2.820 μs)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              22445.684  (22421.564 .. 22476.748)
      y                  431.800    (-42867.246 .. 41837.287)
    variance introduced by outliers: 71% (severely inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/r0
    time                 29.26 ms   (28.93 ms .. 29.60 ms)
                         0.999 R²   (0.998 R² .. 1.000 R²)
    mean                 29.11 ms   (28.71 ms .. 29.40 ms)
    std dev              803.1 μs   (535.5 μs .. 1.243 ms)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              2.252e7    (2.250e7 .. 2.253e7)
      y                  -47827.346 (-190591.748 .. 123102.197)

    benchmarking json-validator/Automaton/testEvent[1000]/evalList
    time                 31.08 ms   (30.27 ms .. 31.95 ms)
                         0.997 R²   (0.995 R² .. 0.999 R²)
    mean                 30.47 ms   (29.83 ms .. 31.06 ms)
    std dev              1.048 ms   (691.9 μs .. 1.661 ms)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              2.254e7    (2.252e7 .. 2.256e7)
      y                  -69071.824 (-215973.254 .. 122067.913)
    variance introduced by outliers: 11% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parList
    time                 166.0 ms   (161.3 ms .. 170.4 ms)
                         0.999 R²   (0.998 R² .. 1.000 R²)
    mean                 166.8 ms   (165.0 ms .. 169.9 ms)
    std dev              3.026 ms   (1.617 ms .. 4.417 ms)
    allocated:           0.999 R²   (0.997 R² .. 1.000 R²)
      iters              2.821e7    (2.730e7 .. 2.887e7)
      y                  -660489.143 (-3851708.098 .. 2423597.073)
    variance introduced by outliers: 12% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 2
    time                 136.7 ms   (134.0 ms .. 141.9 ms)
                         0.998 R²   (0.993 R² .. 1.000 R²)
    mean                 138.0 ms   (135.9 ms .. 141.2 ms)
    std dev              3.547 ms   (2.125 ms .. 5.066 ms)
    allocated:           1.000 R²   (0.999 R² .. 1.000 R²)
      iters              2.292e7    (2.258e7 .. 2.321e7)
      y                  -185341.714 (-1527383.850 .. 1970815.492)
    variance introduced by outliers: 11% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 4
    time                 133.1 ms   (126.1 ms .. 139.7 ms)
                         0.998 R²   (0.993 R² .. 1.000 R²)
    mean                 137.0 ms   (134.9 ms .. 139.0 ms)
    std dev              2.880 ms   (2.066 ms .. 3.884 ms)
    allocated:           1.000 R²   (0.999 R² .. 1.000 R²)
      iters              2.283e7    (2.245e7 .. 2.319e7)
      y                  -155456.000 (-1201064.986 .. 1943704.825)
    variance introduced by outliers: 12% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 8
    time                 139.1 ms   (135.7 ms .. 145.0 ms)
                         0.999 R²   (0.995 R² .. 1.000 R²)
    mean                 139.2 ms   (137.3 ms .. 141.5 ms)
    std dev              2.784 ms   (1.583 ms .. 4.372 ms)
    allocated:           0.999 R²   (0.999 R² .. 1.000 R²)
      iters              2.274e7    (2.233e7 .. 2.322e7)
      y                  -129970.286 (-1533822.194 .. 1870080.747)
    variance introduced by outliers: 12% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 16
    time                 141.8 ms   (138.0 ms .. 147.8 ms)
                         0.999 R²   (0.998 R² .. 1.000 R²)
    mean                 138.5 ms   (135.4 ms .. 140.2 ms)
    std dev              2.954 ms   (1.224 ms .. 4.045 ms)
    allocated:           1.000 R²   (0.999 R² .. 1.000 R²)
      iters              2.281e7    (2.258e7 .. 2.330e7)
      y                  -374372.571 (-2704371.529 .. 1002356.421)
    variance introduced by outliers: 12% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 100
    time                 137.1 ms   (132.4 ms .. 140.8 ms)
                         0.999 R²   (0.995 R² .. 1.000 R²)
    mean                 138.4 ms   (136.1 ms .. 141.8 ms)
    std dev              4.352 ms   (2.137 ms .. 6.922 ms)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              2.289e7    (2.274e7 .. 2.307e7)
      y                  -64529.143 (-829303.586 .. 790731.706)
    variance introduced by outliers: 11% (moderately inflated)

![Transitions Diagram](transitions.png)
