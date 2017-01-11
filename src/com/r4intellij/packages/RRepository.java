package com.r4intellij.packages;


public class RRepository {
  private final String myUrl;

  public RRepository(String url) {
    this.myUrl = url;
  }

  public String getUrl() {
    return myUrl;
  }

  @Override
  public String toString() {
    return myUrl;
  }
}
