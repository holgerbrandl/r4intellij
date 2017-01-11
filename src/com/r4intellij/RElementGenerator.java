package com.r4intellij;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.impl.PsiFileFactoryImpl;
import com.intellij.testFramework.LightVirtualFile;

public class RElementGenerator {

  public static PsiFile createDummyFile(String contents, boolean physical, Project project) {
    final PsiFileFactory factory = PsiFileFactory.getInstance(project);
    final String name = "dummy." + RFileType.INSTANCE.getDefaultExtension();
    final LightVirtualFile virtualFile = new LightVirtualFile(name, RFileType.INSTANCE, contents);
    final PsiFile psiFile = ((PsiFileFactoryImpl)factory).trySetupPsiForFile(virtualFile, RLanguage.getInstance(), physical, true);
    assert psiFile != null;
    return psiFile;
  }
}
