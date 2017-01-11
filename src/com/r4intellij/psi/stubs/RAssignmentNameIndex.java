package com.r4intellij.psi.stubs;

import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.stubs.StubIndexKey;
import com.r4intellij.psi.api.RAssignmentStatement;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

public class RAssignmentNameIndex extends StringStubIndexExtension<RAssignmentStatement> {
  public static final StubIndexKey<String, RAssignmentStatement> KEY = StubIndexKey.createIndexKey("R.function.shortName");

  @Override
  @NotNull
  public StubIndexKey<String, RAssignmentStatement> getKey() {
    return KEY;
  }

  public static Collection<RAssignmentStatement> find(String name, Project project, GlobalSearchScope scope) {
    return StubIndex.getElements(KEY, name, project, scope, RAssignmentStatement.class);
  }

  public static Collection<String> allKeys(Project project) {
    return StubIndex.getInstance().getAllKeys(KEY, project);
  }
}
