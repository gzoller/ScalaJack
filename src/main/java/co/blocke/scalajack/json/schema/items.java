package co.blocke.scalajack.json.schema;

import java.lang.annotation.*;

@Target({ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface items{
    String value();
}
