# Performance

JSON serialization benchmarks I found in various repos often measured (IMO) silly things like how fast a parser 
could handle a small list of Int.  For this benchmark I used a more substantial model + JSON.  It's still 
not large by any measure, but it does have some nested objects and collections that make it a more
realistic test.

The test is run via jmh, a popular benchmarking tool.  The JVM is stock--not tuned to within an inch
of its life, again to be a more realistic use case.

Run benchmark from the ScalaJack/benchmark directory (not the main ScalaJack project directory): 
```
sbt "jmh:run -i 10 -wi 10 -f 2 -t 1 co.blocke.*"
```

## Reading Performance:

| Benchmark        | Mode  | Count  |           Score |        Error | Units |
|------------------|-------|-------:|----------------:|-------------:|-------|
| Jsoniter         | thrpt |  20    |     987991.329  |  ±  6645.992 | ops/s |
| **ScalaJack 8 (fast mode)**  | thrpt |  20    |   **642235.553**|  ± 10394.860 | ops/s |
| ZIOJson          | thrpt |  20    |     586716.228  |  ±  2542.783 | ops/s |
| **ScalaJack 8 (easy mode)**  | thrpt |  20    |   **426718.318**|  ±   692.828 | ops/s |
| Circe            | thrpt |  20    |     266568.198  |  ±  5695.754 | ops/s |
| Play             | thrpt |  20    |     207737.560  |  ±   842.108 | ops/s |
| Argonaut         | thrpt |  20    |     197876.777  |  ± 11181.751 | ops/s |

## Writing Performance:

| Benchmark        | Mode  | Count  |           Score |        Error | Units |
|------------------|-------|-------:|----------------:|-------------:|-------|
| Jsoniter         | thrpt |  20    |     2843150.452 |  ± 21478.503 | ops/s |
| Hand-Tooled      | thrpt |  20    |     2732571.374 |  ± 15129.007 | ops/s |
| Circe            | thrpt |  20    |     1958244.437 |  ± 23965.817 | ops/s |
|**ScalaJack 8**   | thrpt |  20    | **1729426.328** |  ±  4484.721 | ops/s |
| ZIO JSON         | thrpt |  20    |      794352.301 |  ± 32336.852 | ops/s |
| Argonaut         | thrpt |  20    |      690269.697 |  ±  6348.882 | ops/s |
| Play JSON        | thrpt |  20    |      438650.022 |  ± 23800.221 | ops/s |

### Interpretation

Performance for ScalaJack has been... a journey.  As I've explored the population of serializers
available for Scala, of which this is a sample of popular choices, I've observed "generations"
of designs.  ScalaJack 8 has grown through each successive generation.

Focusing originally on write performance, my original design was very attuned to the internal
structure of ScalaJack.  The code was clean and beautiful--and slow!  I was able to get a write
score of only 30-50000, vs Circe, my original benchmark, which was just under 2 million.  Not a 
good showing.  After an extensive overhaul and re-think, performance peaked at the 1.7 million
mark, which I was happy with.  That put ScalaJack ahead of everyone else except Circe.  Then
something unexpected happened...

I tried Jsoniter, and was dumbstruck when it outperformed hand-tooled code, which I expected
would be a natural theoretical maximum write performance.  How can a full serializer, having
whatever logic, be *faster* than hand-tooled code with zero logic?!  This breakout level of 
performance for Jsoniter continued on the read tests, being roughly 4x faster than most and 
2x the level of ZIO-Json, the previous front-runner.  How?!

I observed the older serializers processed JSON to/from an AST and used conventional JSON
parsing techniques; basically fortified editions of a simple JSON parser.  ZIO-Json's 
impressive read performance wasn't achieved by any one thing, but rather a collection of well-
applied techniques, including *not* using an intermediate AST.  So naturally I incorporated
some of ZIO-Json's approach (and a bit of their code) for JSON reading, stripped, refitted, 
and adapted to ScalaJack, and read performance jumped to 633K.  Nice!

Jsoniter, it turns out, achieves its neck-breaking speed by going deep--very deep.  They
use a lot of low level byte arrays and bitwise operators, much as you'd expect in a C program,
to improve on the standard library functions everyone else uses.  It works.

ScalaJack's focus is first and foremost to be frictionless--no drama to the user.  ScalaJack requires 
zero boilerplate--you can throw any Scala object (or even a Java object) at it with no pre-preparation 
and it will serialize it.  For its intended use-cases, ScalaJack offers performance equal
to, or exceeding, several widely-used alternative choices.
