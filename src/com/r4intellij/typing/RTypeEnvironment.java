package com.r4intellij.typing;

import com.r4intellij.typing.types.RType;
import com.r4intellij.typing.types.RUnknownType;

import java.util.HashMap;
import java.util.Map;

public class RTypeEnvironment {
  private Map<String, RType> nameToType = new HashMap<String, RType>();

  public void addType(String name, RType type) {
    nameToType.put(name, type);
  }

  public RType getType(String name) {
    if (nameToType.containsKey(name)) {
      return nameToType.get(name);
    }
    return RUnknownType.INSTANCE;
  }

  public boolean contains(String name) {
    return nameToType.containsKey(name);
  }
}
