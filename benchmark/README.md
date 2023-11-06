# Performance

JSON serialization benchmarks I found in various repos often measured (IMO) silly things like how fast a parser 
could handle a small list of Int.  For this benchmark I used a more substantial model + JSON.  It's still 
not large by any measure, but it does have some nested objects and collections that make it a more
realistic test.

The test is run via jmh, a popular benchmarking tool.  The JVM is stock--not tuned to within an inch
of its life, again to be a more realistic use case.

Run benchmark from the ScalaJack/benchmark directory (not the main ScalaJack project directory): 
```
sbt "jmh:run -i 10 -wi 10 -f 2 -t 1 co.blocke.WritingBenchmark"
```

## Writing Performance:

| Benchmark        | Mode  | Count |       Score |        Error | Units |
|------------------|-------|----:|------------:|-------------:|-------|
| Hand-Tooled      | thrpt |  20 | 2,575,393.513 | ± 178731.952 | ops/s |
| Circe            | thrpt |  20 | 1,939,339.085 | ±   6279.547 | ops/s |
|**ScalaJack 8**     | thrpt |  20 | **1,703,256.521** | ±  12260.518 | ops/s |
| ZIO JSON         | thrpt |  20 |  818,228.736 | ±   3070.298 | ops/s |
| Argonaut         | thrpt |  20 |  716,228.404 | ±   6241.145 | ops/s |
| Play JSON        | thrpt |  20 |  438,538.475 | ±  16319.198 | ops/s |
| ScalaJack 7      | thrpt |  20 |  106,292.338 | ±    330.111 | ops/s |

### Interpretation

The Hand-Tooled case is creating JSON manually in code.  I included it to show the presumed 
upper-bound of achievable performance. No serializer, with whatever logic it must do, can be faster
than hand-tooled code that is hard-wired in its output and requires zero logic.

We see in these results that both Circe and ScalaJack are very close in performance--close to each 
other and very close to the performance of hand-tooled code.

Circe is the gold standard for JSON serializers due to its many features, excellent performance, and
widespread adoption.  The one cost Circe imposes is the same one virtually all other serializers
require: boilerplate must be provided to define encoders/decoders to aid the serializion.  Circe's
boilerplate is actually not terrible.  Others require a fair bit of extra code per class serialized. 

ScalaJack's focus is first and foremost to be frictionless--no drama to the user.  The very slight 
difference in maximal performance is a worthy expense--its still blazing fast.  ScalaJack requires 
zero boilerplate--you can throw any Scala object (or even a Java object) at it with no pre-preparation 
and it will serialize it.  You'll notice the enormous performange improvement ScalaJack 8 has over 
ScalaJack 7, due to moving everything possible into compile-time macros for speed.
