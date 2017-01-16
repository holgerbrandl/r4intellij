package com.r4intellij.run.debug;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.Processor;
import com.intellij.xdebugger.XDebuggerUtil;
import com.r4intellij.RFileType;
import com.r4intellij.parsing.RElementTypes;
import org.jetbrains.annotations.NotNull;

final class RLineBreakpointUtils {

  public static boolean canPutAt(@NotNull final Project project, @NotNull final VirtualFile file, final int line) {
    return isRFile(file) && isStoppable(project, file, line);
  }

  private static boolean isRFile(@NotNull final VirtualFile file) {
    final String defaultExtension = RFileType.INSTANCE.getDefaultExtension();
    final String extension = file.getExtension();

    return defaultExtension.equalsIgnoreCase(extension);
  }

  private static boolean isStoppable(@NotNull final Project project, @NotNull final VirtualFile file, final int line) {
    final PsiFile psiFile = PsiManager.getInstance(project).findFile(file);

    if (psiFile == null) return false;

    final Document document = PsiDocumentManager.getInstance(project).getDocument(psiFile);

    if (document == null) return false;

    final boolean[] justResult = new boolean[]{false};

    XDebuggerUtil.getInstance().iterateLine(
      project,
      document,
      line,
      new Processor<PsiElement>() {
        @Override
        public boolean process(@NotNull final PsiElement element) {
          if (isNotStoppable(element) || isNotStoppable(element.getNode().getElementType())) return true;

          justResult[0] = true;
          return false;
        }
      }
    );

    return justResult[0];
  }

  private static boolean isNotStoppable(@NotNull final PsiElement element) {
    return element instanceof PsiWhiteSpace || element instanceof PsiComment;
  }

  private static boolean isNotStoppable(@NotNull final IElementType type) {
      return type == RElementTypes.R_LBRACE || type == RElementTypes.R_RBRACE;
  }
}
