## View / SpliceInto

The use case for view/spliceInto is where you have a master class, which may contain some system-private information.  We may also need a 'lite' version of the master class for transport to a UI, but the lite version must not contain any of the private information.  We want the lite version to be a "view" (projection) of the master class.  If the UI modifies the lite version, we want to splice its changes back into the master.

First the classes:
```scala
case class Master(
    name:          String,
    id:            Long,
    underwearSize: Char,
    favorites:     List[String]
    )

case class Subset(
    name:          String,
    id:            Long,
    favorites:     List[String]
    )
```

Here we have a Subset class that doesn't include the person's underwearSize.  Now let's see how to project that view:

```scala
val master = Master("Fred",123L, 'M', List("music","football"))
val subset = sj.view[Subset](master)
// subset = Subset("Fred", 123L, List("music","football"))
```

We now have a Subset object that's "safe" to transport.

Now let's assume that something has modified Subset and we want to recombine it back with Master.  We can do it this way:

```scala
val newMaster = sj.spliceInto(modifiedSubset, master)
```

Easy, right?

