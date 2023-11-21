# Performance

JSON serialization benchmarks I found in various repos often measured (IMO) silly things like how fast 
a parser could handle a small list of Int.  For this benchmark I used a more substantial model + JSON.  
It's still a small model, but it does have some nested objects and collections that make it a more
realistic test.

The test is run via jmh, a common and accepted benchmarking tool.  The JVM is **stock**--not tuned to 
within an inch of its life, again to be a more realistic use case.

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

![image info](benchmark/Writing Performance.png)

| Benchmark        | Mode  | Count  |           Score |        Error | Units |
|------------------|-------|-------:|----------------:|-------------:|-------|
|**ScalaJack 8 (fast mode)**   | thrpt |  20    | **3039273.222** |  ± 14952.932 | ops/s |
| Jsoniter         | thrpt |  20    |     2843150.452 |  ± 21478.503 | ops/s |
| Hand-Tooled      | thrpt |  20    |     2732571.374 |  ± 15129.007 | ops/s |
| Circe            | thrpt |  20    |     1958244.437 |  ± 23965.817 | ops/s |
|**ScalaJack 8 (easy mode)**   | thrpt |  20    | **846484.100** |  ± 1123.204 | ops/s |
| ZIO JSON         | thrpt |  20    |      794352.301 |  ± 32336.852 | ops/s |
| Argonaut         | thrpt |  20    |      690269.697 |  ±  6348.882 | ops/s |
| Play JSON        | thrpt |  20    |      438650.022 |  ± 23800.221 | ops/s |

**Note:** Exact numbers aren't terribly important--they will vary depending on the platform
used.  The important thing is the relative relationship between libraries given all tests
were performed on the same platform.

### Interpretation

Performance for ScalaJack has been a journey.  ScalaJack is a mature product, and while it
was once (a long time ago) quite fast vs its competition, its performance has lagged
considerably.  ScalaJack 8 changes that!  

I was sampling and testing against a collection of popular serializers for Scala util
something quite unexpected happend.  When I tested Jsoniter, its performance was through
the roof!  Even faster than hand-tooled code.  This was a shock.  I had to learn how this
worked.

So full credit where credit is due:  ScalaJack 8's reading/writing codec architecture 
is heavily derived from Jsoniter.

[Jsoniter's License](https://github.com/plokhotnyuk/jsoniter-scala/blob/af23cf65a70d48834b8fecb792cc333b23409c6f/LICENSE)

There are a number of optimizations and design choices I elected not to bring over from
Jsoniter, and of course ScalaJack utilizes our own scala-reflection library to great effect.

Jsoniter, it turns out, achieves its neck-breaking speed by going deep--very deep.  They
use a lot of low level byte arrays and bitwise operators, much as you'd expect to see in 
a C program, to improve on the standard library functions everyone else uses.  It works.

ScalaJack's focus is first and foremost to be frictionless--no drama to the user.  ScalaJack requires 
zero boilerplate--you can throw any Scala object (or even a Java object) at it with no pre-preparation 
and it will serialize it.  For its intended use-cases, ScalaJack offers excellent performance, equal
to or exceeding a number of widely-used alternative choices.

### Technical Notes

Achieving extreme speed for ScalaJack was weeks of learning, trial, error,
and re-writes.  I studied Jsoniter, Circe, and ZIO Json, and others to learn optimizations.
The tough news for anyone wanting to duplicate this kind of performance in your own code 
is that there isn't one magic trick to achieve maximum performance.  It's a basket 
of techniques, each achieving marginal gains that add up, and you must decide when enough
is enough.  Here's a partial list of learnings incorporated into ScalaJack:

* Being careful when using .asInstanceOf[]... in fact try to avoid it wherever possible 
  as it messes up CPU cache harming performance.  This means a lot of very careful type
  management, and its why you see the RTypeRefs from scala-reflection are now all typed
  in the latest version

* Lots of specific typing.  Don't make the computer think--provide detailed types wherever 
  you can

* For macro-based software like this--find every opportunity to do hard work at 
  compile-time

* Be mindful of what code your macros generate!  You can paint by the numbers with quotes and
  splices, like the documentaion and blogs suggest, and you'll get something working. When you
  examine the code this produces, you may be disappointed. If it looks kludgy it will be slow--rework
  your macros until the code is smooth.  For fastest performance you'll actually have to generate
  custom functions as shown in ScalaJack's code (look at JsonCodecMaker.scala)
