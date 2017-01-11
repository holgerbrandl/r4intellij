package com.r4intellij.typing.types;

import com.r4intellij.typing.RTypeEnvironment;

public class RTypeVariable extends RType {
  private final String myVarName;

  @Override
  public String getCanonicalName() {
    return myVarName;
  }

  @Override
  public RType resolveType(RTypeEnvironment env) {
    RType substitutionType = env.getType(myVarName);
    substitutionType = substitutionType.replaceS3Types(getS3Classes()); // push down s3
    return substitutionType.resolveType(env);
  }

  public RTypeVariable(String name) {
    myVarName = name;
  }
}
