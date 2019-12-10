package co.blocke.series60;

import java.lang.annotation.*;

@Inherited
@Target({ElementType.PARAMETER, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface DBKey {
	// Some indexing schemes create unordered composite keys (eg Mongo) while others have primary/secondary distinctions (eg Dynamo).
	// If the later, use the index parameter to "order" fields within a compound key.
	int index() default 0;
}
