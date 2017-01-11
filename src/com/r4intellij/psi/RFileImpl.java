package com.r4intellij.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.icons.AllIcons;
import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.RFileType;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class RFileImpl extends PsiFileBase implements RFile {

  public RFileImpl(@NotNull final FileViewProvider viewProvider) {
    this(viewProvider, RFileType.INSTANCE.getLanguage());
  }

  public RFileImpl(@NotNull final FileViewProvider viewProvider, @NotNull final Language language) {
    super(viewProvider, language);
  }

  @Override
  @NotNull
  public FileType getFileType() {
    return RFileType.INSTANCE;
  }

  public String toString() {
    return "RFile:" + getName();
  }

  @Override
  public Icon getIcon(int flags) {
    return AllIcons.FileTypes.Text;   // TODO: icon
  }

  @Override
  public void accept(@NotNull final PsiElementVisitor visitor) {
    visitor.visitFile(this);
  }
}
