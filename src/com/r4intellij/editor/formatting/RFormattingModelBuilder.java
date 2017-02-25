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
import com.intellij.psi.tree.TokenSet;
import com.r4intellij.RLanguage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.r4intellij.parsing.RElementTypes.*;

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
//        Indent indent = Indent.getNoneIndent();
        Indent indent = Indent.getNoneIndent();
//        Wrap wrap = Wrap.createWrap(WrapType.CHOP_DOWN_IF_LONG, false);
        Wrap wrap = null;
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
//                .between(R_FUNCTION_EXPRESSION, R_FUNCTION_EXPRESSION).
//                        blankLines(commonSettings.BLANK_LINES_AROUND_METHOD)

//                .after(FUNCTION_DECLARATION).blankLines(commonSettings.BLANK_LINES_AROUND_METHOD)
//                .after(R_FUNCTION_EXPRESSION).blankLines(commonSettings.BLANK_LINES_AROUND_METHOD)

//                .between(RElementTypes.R_OPERATOR, RElementTypes.R_EXPRESSION).spaces(1)
//                .between(R_EXPRESSION, ASSIGNMENTS).spaces(1)
//                .between(ASSIGNMENTS, R_EXPRESSION).spaces(1)
//                .between(R_OPERATOR_EXPRESSION, R_OPERATOR).spaces(1)
//                .between(R_OPERATOR_EXPRESSION, R_OPERATOR).spaces(1)
//                .between(R_EXPRESSION, R_EXPRESSION).spaces(1)
//                .between(R_EXPRESSION, R_OPERATOR).spaces(1)
//                .between(R_NUMERIC_LITERAL_EXPRESSION, R_OPERATOR).spaces()
//                .around(ASSIGNMENTS)
//                .around(ASSIGNMENTS).spaces(1)
                //todo make this a preference or inhereit from other languages
                .after(TokenSet.create(R_IF, R_ELSE, R_FOR, R_WHILE)).spaces(1)

                .before(R_LBRACE).spaces(1) // if (%){
                .around(R_OPERATOR).spaces(1)
                .between(R_RPAR, R_CALL_EXPRESSION).spaces(1)
                .between(R_RPAR, R_BLOCK_EXPRESSION).spaces(1)

                .around(TokenSet.create(R_RBRACE, R_LPAR, R_RPAR, R_LBRACKET, R_RBRACKET)).spaces(0)

//                .around(R_LPAR).spaces(0)
////                .after(COLON).spacing(spacesAfterColon, spacesAfterColon, 0, false, 0)
//                .withinPair(R_LBRACE, R_RBRACE).spaceIf(commonSettings.SPACE_WITHIN_BRACKETS, true)
//                .withinPair(L_CURLY, R_CURLY).spaceIf(commonSettings.SPACE_WITHIN_BRACES, true)
//                .before(COMMA).spacing(spacesBeforeComma, spacesBeforeComma, 0, false, 0)
                .after(R_COMMA).spaceIf(commonSettings.SPACE_AFTER_COMMA)
                ;
    }


    @Nullable
    public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset) {
        return null;
    }
}
