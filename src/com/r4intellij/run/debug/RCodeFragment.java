package com.r4intellij.run.debug;

import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiManager;
import com.intellij.psi.SingleRootFileViewProvider;
import com.intellij.psi.impl.PsiManagerEx;
import com.intellij.psi.impl.file.impl.FileManager;
import com.intellij.testFramework.LightVirtualFile;
import com.r4intellij.psi.RFileImpl;
import org.jetbrains.annotations.NotNull;

class RCodeFragment extends RFileImpl {

  public RCodeFragment(@NotNull final Project project, @NotNull final String name, @NotNull final String text) {
    super(createLightVirtualFileViewProvider(project, name, text));

    ((SingleRootFileViewProvider)getViewProvider()).forceCachedPsi(this);
  }

  @NotNull
  private static FileViewProvider createLightVirtualFileViewProvider(@NotNull final Project project,
                                                                     @NotNull final String name,
                                                                     @NotNull final String text) {
    return getFileManager(project).createFileViewProvider(
      createLightVirtualFile(name, text), true
    );
  }

  @NotNull
  private static FileManager getFileManager(@NotNull final Project project) {
    return ((PsiManagerEx)PsiManager.getInstance(project)).getFileManager();
  }

  @NotNull
  private static LightVirtualFile createLightVirtualFile(@NotNull final String name, @NotNull final String text) {
    return new LightVirtualFile(name, getFileType(name), text);
  }

  @NotNull
  private static FileType getFileType(@NotNull final String name) {
    return FileTypeManager.getInstance().getFileTypeByFileName(name);
  }
}
