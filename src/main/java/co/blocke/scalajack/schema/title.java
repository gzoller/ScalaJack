package co.blocke.scalajack.schema;

import java.lang.annotation.*;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface title{
    String value();
}
