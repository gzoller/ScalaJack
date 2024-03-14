package co.blocke.scalajack.schema;

import java.lang.annotation.*;

@Target({ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface pattern{
    String value();
}
