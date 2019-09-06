package co.blocke.scalajack;

import java.lang.annotation.*;

@Inherited
@Target({ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface JSSdependencies {
    String[] dependsOn();
}
