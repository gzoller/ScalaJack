package co.blocke.scalajack;

import java.util.Optional;

public class Maybe2 {

    private String one = "foom";
    public String getOne(){ return one; }
    public void setOne(String v) { one = v; }

    private Optional<String> two = Optional.of("stuff");
    public Optional<String> getTwo(){ return two; }
    public void setTwo(Optional<String> v) { two = v; }

}
