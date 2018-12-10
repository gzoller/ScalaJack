package co.blocke.series7;

import java.lang.annotation.*;

@Target({ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface DBKey {
	// Some indexing schemes create unordered composite keys (eg Mongo) while others have primary/secondary distinctions (eg Dynamo).
	// If the later, use the index parameter to "order" fields within a compound key.
	int index() default 0;
}
