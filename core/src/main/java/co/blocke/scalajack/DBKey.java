package co.blocke.scalajack;

import java.lang.annotation.*;

@Target({ElementType.PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface DBKey {
	String info = "";
}

/*
	Annotations:

		DBKey
		Column("name")
*/