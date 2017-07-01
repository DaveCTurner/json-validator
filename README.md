# json-validator

Sample results of `stack build --file-watch --exec 'json-validator-exe --regress allocated:iters +RTS -T'`:

    benchmarking aeson/testEvent
    time                 36.33 μs   (35.42 μs .. 37.01 μs)
                         0.996 R²   (0.994 R² .. 0.997 R²)
    mean                 36.53 μs   (35.70 μs .. 37.39 μs)
    std dev              2.870 μs   (2.448 μs .. 3.732 μs)
    allocated:           1.000 R²   (1.000 R² .. 1.000 R²)
      iters              84076.240  (84051.873 .. 84101.462)
      y                  -3656.513  (-40038.214 .. 33734.790)
    variance introduced by outliers: 76% (severely inflated)

    benchmarking json-validator/Automaton/testEvent
    time                 7.888 μs   (7.848 μs .. 7.939 μs)
                         0.999 R²   (0.999 R² .. 1.000 R²)
    mean                 7.944 μs   (7.873 μs .. 8.042 μs)
    std dev              303.1 ns   (221.9 ns .. 415.3 ns)
    allocated:           0.985 R²   (0.976 R² .. 0.990 R²)
      iters              216.588    (211.108 .. 222.327)
      y                  -2119.219  (-27923.681 .. 27856.185)
    variance introduced by outliers: 48% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/r0
    time                 7.919 ms   (7.720 ms .. 8.126 ms)
                         0.997 R²   (0.993 R² .. 0.999 R²)
    mean                 8.223 ms   (8.060 ms .. 8.677 ms)
    std dev              710.8 μs   (254.2 μs .. 1.369 ms)
    allocated:           0.996 R²   (0.994 R² .. 0.998 R²)
      iters              281553.485 (274415.686 .. 288885.041)
      y                  -32443.540 (-166877.654 .. 81905.589)
    variance introduced by outliers: 50% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/evalList
    time                 8.058 ms   (7.969 ms .. 8.163 ms)
                         0.999 R²   (0.997 R² .. 0.999 R²)
    mean                 8.048 ms   (7.992 ms .. 8.138 ms)
    std dev              193.4 μs   (140.1 μs .. 297.8 μs)
    allocated:           0.996 R²   (0.994 R² .. 0.998 R²)
      iters              305654.949 (299064.277 .. 312536.491)
      y                  -31058.694 (-153700.454 .. 84396.142)

    benchmarking json-validator/Automaton/testEvent[1000]/parList
    time                 28.52 ms   (23.04 ms .. 33.66 ms)
                         0.892 R²   (0.831 R² .. 0.953 R²)
    mean                 25.30 ms   (21.90 ms .. 27.88 ms)
    std dev              6.318 ms   (4.858 ms .. 9.150 ms)
    allocated:           0.969 R²   (0.938 R² .. 0.985 R²)
      iters              313346.584 (286765.972 .. 338235.240)
      y                  -97110.327 (-367121.541 .. 174639.232)
    variance introduced by outliers: 82% (severely inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 2
    time                 27.42 ms   (22.56 ms .. 34.48 ms)
                         0.858 R²   (0.761 R² .. 0.949 R²)
    mean                 26.76 ms   (24.58 ms .. 29.20 ms)
    std dev              5.591 ms   (4.604 ms .. 7.030 ms)
    allocated:           0.997 R²   (0.994 R² .. 0.999 R²)
      iters              622755.979 (603460.653 .. 640412.081)
      y                  -2308.915  (-137128.286 .. 158881.467)
    variance introduced by outliers: 77% (severely inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 4
    time                 27.94 ms   (26.13 ms .. 29.90 ms)
                         0.985 R²   (0.968 R² .. 0.993 R²)
    mean                 27.83 ms   (26.76 ms .. 29.23 ms)
    std dev              2.708 ms   (1.839 ms .. 3.870 ms)
    allocated:           0.987 R²   (0.980 R² .. 0.994 R²)
      iters              571039.711 (529279.125 .. 602680.900)
      y                  -58045.699 (-370095.954 .. 333680.844)
    variance introduced by outliers: 43% (moderately inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 8
    time                 27.10 ms   (23.50 ms .. 31.14 ms)
                         0.940 R²   (0.898 R² .. 0.980 R²)
    mean                 27.07 ms   (25.52 ms .. 28.88 ms)
    std dev              3.746 ms   (3.136 ms .. 4.800 ms)
    allocated:           0.987 R²   (0.977 R² .. 0.994 R²)
      iters              536945.445 (507436.156 .. 562840.062)
      y                  -68551.059 (-361681.695 .. 237850.776)
    variance introduced by outliers: 59% (severely inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 16
    time                 26.80 ms   (23.73 ms .. 28.96 ms)
                         0.964 R²   (0.929 R² .. 0.989 R²)
    mean                 26.74 ms   (25.07 ms .. 28.96 ms)
    std dev              3.955 ms   (2.704 ms .. 5.454 ms)
    allocated:           0.993 R²   (0.987 R² .. 0.997 R²)
      iters              513373.977 (487740.383 .. 535095.874)
      y                  -44151.007 (-272849.325 .. 254751.415)
    variance introduced by outliers: 60% (severely inflated)

    benchmarking json-validator/Automaton/testEvent[1000]/parListChunk 100
    time                 23.94 ms   (22.15 ms .. 25.90 ms)
                         0.969 R²   (0.939 R² .. 0.988 R²)
    mean                 24.70 ms   (23.58 ms .. 25.78 ms)
    std dev              2.580 ms   (1.995 ms .. 3.328 ms)
    allocated:           0.988 R²   (0.976 R² .. 0.995 R²)
      iters              600388.028 (568109.959 .. 627714.925)
      y                  -70297.965 (-407450.114 .. 322817.382)
    variance introduced by outliers: 46% (moderately inflated)

![Transitions Diagram](transitions.png)
