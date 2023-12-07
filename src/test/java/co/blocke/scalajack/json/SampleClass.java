package co.blocke.scalajack.json;

import co.blocke.scala_reflection.Ignore;

public class SampleClass {
    // Fields
    private String name;
    private int age;
    private String address;

    // Constructors
    // public SampleClass(){}

    public SampleClass(String name, int age, String address) {
        this.name = name;
        this.age = age;
        this.address = address;
    }

    // Getter methods
    public String getName() {
        return name;
    }

    @Ignore
    public int getAge() {
        return age;
    }

    public String getAddress() {
        return address;
    }

    // Setter methods (optional, depending on whether you want to allow modification)
    public void setName(String name) {
        this.name = name;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public void setAddress(String address) {
        this.address = address;
    }

}