package co.blocke.scalajack;

public class JavaTestClass2 {
    String name = "Mike";
    int id;

    public int count;
    private int cantSee;

    public JavaTestClass2() {}

    @Ignore public String getName() {
        return name;
    }
    public void setName(String newName) {
        this.name = newName;
    }

    public int getId() {
        return id;
    }
    public void setId(int id) {
        this.id = id;
    }
}