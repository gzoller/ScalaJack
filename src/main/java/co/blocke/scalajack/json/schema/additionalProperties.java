package co.blocke.scalajack.json.schema;

import java.lang.annotation.*;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface additionalProperties{
    String value();
}
