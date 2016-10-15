NOTE:

There are 3 cases of "plain" (non-case) classes:

1) Scala classes having val designated parameters in its constructor
2) Scala classes having either/both of: getter/setter or public var attributes (with 0-arg constructor!)
3) Java classes

Case 1 is not really tested much--because... Once this case is identifed, a normal CaseClassTypeAdapter is
constructed and from that point on it can be handled as a case class normally would.  Since case classes
are tested extensively elsewhere, there's no value in repeating those tests here.

For other tests of 2) and 3) look for model classes of <name>Mix and <name>Java respectively.