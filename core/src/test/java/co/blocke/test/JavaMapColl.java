package co.blocke.test;

import java.util.*;
import co.blocke.scalajack.Ignore;

public class JavaMapColl extends JavaCollBase {
    private HashMap<String,Integer> items;
    private TreeMap<String,Integer> tree;
    private int age;

    public WeakHashMap<String,Integer> weakling;

    @Ignore public int getAge() { return age; }
    public void setAge(int a) { age = a; }
    public HashMap<String,Integer> getItems() { return items; }
    public void setItems(HashMap<String,Integer> i) { items = i; }
    public TreeMap<String,Integer> getTree() { return tree; }
    public void setTree(TreeMap<String,Integer> i) { tree = i; }
}
