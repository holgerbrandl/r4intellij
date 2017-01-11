package com.r4intellij.typing.types;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.Function;
import com.r4intellij.typing.RTypeEnvironment;

import java.util.List;

public class RTypeSequence extends RType {
  private List<RType> myTypes;

  public RTypeSequence(List<RType> types) {
    myTypes = types;
  }

  @Override
  public String getCanonicalName() {
    return "type sequence: " + StringUtil.join(myTypes, new Function<RType, String>() {
      @Override
      public String fun(RType type) {
        return type.getName();
      }
    }, ",");
  }

  @Override
  public RType resolveType(RTypeEnvironment env) {
    return RType.getMaxType(myTypes, env);
  }
}
