package co.blocke.series4;

import java.lang.annotation.*;

@Target({ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface DBKey {
	String info = "";
}
