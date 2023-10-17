package co.blocke.scalajack;

import java.util.Optional;

public class Maybe {

    private Optional<String> one = Optional.empty();
    public Optional<String> getOne(){ return one; }
    public void setOne(Optional<String> v) { one = v; }

    private Optional<String> two = Optional.of("stuff");
    public Optional<String> getTwo(){ return two; }
    public void setTwo(Optional<String> v) { two = v; }

}
