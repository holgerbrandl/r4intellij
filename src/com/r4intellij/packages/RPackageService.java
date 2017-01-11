package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.openapi.components.*;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;

/**
 * @author avesloguzova
 */
@State(name = "RPackageService",
  storages = {
    @Storage(file = StoragePathMacros.APP_CONFIG + "/rpackages.xml")
  }
)
public class RPackageService implements PersistentStateComponent<RPackageService> {

  public Map<String, String> allPackages = ContainerUtil.newConcurrentMap();
  public int CRANMirror = 1;
  public List<String> enabledRepositories = Lists.newArrayList();
  public List<String> userRepositories = Lists.newArrayList();

  public static RPackageService getInstance() {
    return ServiceManager.getService(RPackageService.class);
  }

  @Nullable
  @Override
  public RPackageService getState() {
    return this;
  }

  @Override
  public void loadState(RPackageService state) {
    XmlSerializerUtil.copyBean(state, this);
  }
}
