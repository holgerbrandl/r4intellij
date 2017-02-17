/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.formatting;

import com.intellij.formatting.*;
import com.intellij.json.JsonLanguage;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.r4intellij.RLanguage;
import com.r4intellij.parsing.RElementTypes;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.intellij.json.JsonElementTypes.*;
import static com.r4intellij.lexer.RTokenTypes.OPERATORS;

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
//        Indent indent = Indent.getAbsoluteNoneIndent();
        Indent indent = Indent.getNoneIndent();
        Wrap wrap = Wrap.createWrap(WrapType.CHOP_DOWN_IF_LONG, false);
//        Alignment alignment = Alignment.createAlignment();
        Alignment alignment = null;

        SpacingBuilder spacingBuilder = createSpacingBuilder(settings);
        return FormattingModelProvider.createFormattingModelForPsiFile(containingFile,
                new RFormatterBlock(astNode, alignment, indent, wrap, settings, spacingBuilder), settings);
    }


    static SpacingBuilder createSpacingBuilder(CodeStyleSettings settings) {
        final RCodeStyleSettings rStyleSettings = settings.getCustomSettings(RCodeStyleSettings.class);
        final CommonCodeStyleSettings commonSettings = settings.getCommonSettings(JsonLanguage.INSTANCE);

        final int spacesBeforeComma = commonSettings.SPACE_BEFORE_COMMA ? 1 : 0;
        final int spacesBeforeColon = rStyleSettings.SPACE_BEFORE_COLON ? 1 : 0;
        final int spacesAfterColon = rStyleSettings.SPACE_AFTER_COLON ? 1 : 0;

        return new SpacingBuilder(settings, RLanguage.getInstance())
                .between(RElementTypes.R_FUNCTION_EXPRESSION, RElementTypes.R_FUNCTION_EXPRESSION).blankLines(commonSettings.BLANK_LINES_AROUND_METHOD)
                .between(RElementTypes.R_FUNCTION_EXPRESSION, RElementTypes.R_FUNCTION_EXPRESSION).blankLines(commonSettings.BLANK_LINES_AROUND_METHOD)
//                .after(FUNCTION_DECLARATION).blankLines(commonSettings.BLANK_LINES_AROUND_METHOD)
                .after(RElementTypes.R_FUNCTION_EXPRESSION).blankLines(commonSettings.BLANK_LINES_AROUND_METHOD)

//                .between(RElementTypes.R_OPERATOR, RElementTypes.R_EXPRESSION).spaces(1)
                .between(RElementTypes.R_OPERATOR_EXPRESSION, RElementTypes.R_OPERATOR).spaces(1)
                .between(RElementTypes.R_OPERATOR_EXPRESSION, RElementTypes.R_OPERATOR).spaces(1)
                .between(RElementTypes.R_EXPRESSION, RElementTypes.R_EXPRESSION).spaces(1)
                .between(RElementTypes.R_EXPRESSION, RElementTypes.R_OPERATOR).spaces(1)
                .between(RElementTypes.R_NUMERIC_LITERAL_EXPRESSION, RElementTypes.R_OPERATOR).spaces(2)
//                .around(ASSIGNMENTS)
                .around(OPERATORS).spaces(1)


//                .before(COLON).spacing(spacesBeforeColon, spacesBeforeColon, 0, false, 0)
//                .after(COLON).spacing(spacesAfterColon, spacesAfterColon, 0, false, 0)
                .withinPair(L_BRACKET, R_BRACKET).spaceIf(commonSettings.SPACE_WITHIN_BRACKETS, true)
                .withinPair(L_CURLY, R_CURLY).spaceIf(commonSettings.SPACE_WITHIN_BRACES, true)
                .before(COMMA).spacing(spacesBeforeComma, spacesBeforeComma, 0, false, 0)
                .after(COMMA).spaceIf(commonSettings.SPACE_AFTER_COMMA);
    }


    @Nullable
    public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset) {
        return null;
    }
}
