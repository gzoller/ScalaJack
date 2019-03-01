package co.blocke.scalajack;

import java.lang.annotation.*;

@Inherited
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface Maybe {
}

