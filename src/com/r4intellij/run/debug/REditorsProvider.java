package com.r4intellij.run.debug;

import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.xdebugger.evaluation.XDebuggerEditorsProviderBase;
import com.r4intellij.RFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

class REditorsProvider extends XDebuggerEditorsProviderBase {

    @NotNull
    private static final String FRAGMENT_NAME = "fragment.r";


    @NotNull
    @Override
    public FileType getFileType() {
        return RFileType.INSTANCE;
    }


    @Override
    protected PsiFile createExpressionCodeFragment(@NotNull final Project project,
                                                   @NotNull final String text,
                                                   @Nullable final PsiElement context,
                                                   final boolean isPhysical) {
        return new RCodeFragment(project, FRAGMENT_NAME, text);
    }
}
