package com.r4intellij.packages;


public class RDefaultRepository extends RRepository {
    int myIndex;


    public RDefaultRepository(String url, int index) {
        super(url);
        myIndex = index;
    }


    public int getIndex() {
        return myIndex;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RDefaultRepository that = (RDefaultRepository) o;
        return myIndex == that.myIndex;
    }


    @Override
    public int hashCode() {
        return myIndex;
    }
}
