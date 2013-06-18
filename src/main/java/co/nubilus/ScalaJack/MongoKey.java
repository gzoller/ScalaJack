package co.nubilus.ScalaJack;

import java.lang.annotation.*;

@Target({ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface MongoKey {
	String info = "";
}
