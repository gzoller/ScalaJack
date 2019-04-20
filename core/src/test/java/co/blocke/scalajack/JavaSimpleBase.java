package co.blocke.scalajack;

public class JavaSimpleBase {

    private int two;
    @DBKey @Change(name="dos") public int getTwo(){ return two; }
    public void setTwo(int v) { two = v; }

    private int three = -10;
    @DBKey(index = 99) @Optional
    public int getThree(){ return three; }
    public void setThree(int v) { three = v; }

    private int bogus = -1;
    public int getBogus(){ return bogus; }
    @Ignore public void setBogus(int v) { bogus = v; }

}
