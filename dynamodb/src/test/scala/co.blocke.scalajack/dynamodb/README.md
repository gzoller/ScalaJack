This test suite is very minimal.

Since the core of the DynamoDB implementation for ScalaJack involves merely using Item's own fromJSON method,
we just wrapped ScalaJack's native JSON capabilities, which have been extensively tested.  There's no particular
need to re-test these here.

There are limitations on what DynamoDB can handle, for example you can't serialize a naked List[Something]
like you can in the JSON flavor.

If DynamoDB can handle it, ScalaJack's DynamoDB flavor should do it.