package co.blocke.scalajack;

import java.lang.annotation.*;

@Inherited
@Target({ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Ignore {
}
