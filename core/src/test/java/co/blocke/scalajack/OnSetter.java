package co.blocke.scalajack;

public class OnSetter {

    private int two;
    public int getTwo(){ return two; }
    @MapName(name="dos") public void setTwo(int v) { two = v; }

}