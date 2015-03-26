/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.formatting;

import com.intellij.formatting.FormattingModel;
import com.intellij.formatting.FormattingModelBuilder;
import com.intellij.formatting.FormattingModelProvider;
import com.intellij.formatting.Indent;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * This code is based on code taken from the bash plugin which took it from the Groovy plugin. :-)
 *
 * @author ilyas, jansorg, brandl
 */
public class RFormattingModelBuilder implements FormattingModelBuilder {

    @NotNull
    @Override
    public FormattingModel createModel(final PsiElement element, final CodeStyleSettings settings) {
        ASTNode node = element.getNode();
        assert node != null;

        PsiFile containingFile = element.getContainingFile();//.getViewProvider().getPsi(RFileType.BASH_LANGUAGE);
        ASTNode astNode = containingFile.getNode();
        assert astNode != null;

        // reenable for release
//        RProjectSettings projectSettings = RProjectSettings.storedSettings(containingFile.getProject());
//        if (!projectSettings.isFormatterEnabled()) {
//            return FormattingModelProvider.createFormattingModelForPsiFile(containingFile, new NoOpBlock(astNode), settings);
//        }

        // see com.intellij.formatting.FormattingModel
        // Typically, a plugin does not need to provide a complete FormattingModel implementation
        return FormattingModelProvider.createFormattingModelForPsiFile(containingFile,
                new RBlock(astNode, null, Indent.getAbsoluteNoneIndent(), null, settings), settings);
    }


    @Nullable
    public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset) {
        return null;
    }
}
