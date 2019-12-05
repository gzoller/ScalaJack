package co.blocke.series60;

import java.lang.annotation.*;

@Inherited
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Optional {
}

