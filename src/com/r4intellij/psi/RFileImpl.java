package com.r4intellij.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.RFileType;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;

public class RFileImpl extends PsiFileBase implements RFile {


    public RFileImpl(FileViewProvider viewProvider) {
        super(viewProvider, RFileType.INSTANCE.getLanguage());
    }


    @Override
    @NotNull
    public FileType getFileType() {
        return RFileType.INSTANCE;
    }


    @Override
    public void accept(@NotNull final PsiElementVisitor visitor) {
        visitor.visitFile(this);
    }
}
