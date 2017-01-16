package com.r4intellij.highlighting;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import com.r4intellij.lexer.RLexer;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.parsing.RParserDefinition;
import com.r4intellij.psi.RPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

public class RHighlighter extends SyntaxHighlighterBase {
    private static final Map<IElementType, TextAttributesKey> ATTRIBUTES = new HashMap<IElementType, TextAttributesKey>();

    static {
        fillMap(ATTRIBUTES, RPsiImplUtil.RESERVED_WORDS, RSyntaxHighlighterColors.KEYWORD);

        fillMap(ATTRIBUTES, RPsiImplUtil.OPERATORS, RSyntaxHighlighterColors.OPERATION_SIGN);

        ATTRIBUTES.put(RElementTypes.R_STRING, RSyntaxHighlighterColors.STRING);
        ATTRIBUTES.put(RElementTypes.R_NUMERIC, RSyntaxHighlighterColors.NUMBER);
        ATTRIBUTES.put(RElementTypes.R_COMPLEX, RSyntaxHighlighterColors.NUMBER);
        ATTRIBUTES.put(RElementTypes.R_INTEGER, RSyntaxHighlighterColors.NUMBER);


        ATTRIBUTES.put(RElementTypes.R_LPAR, RSyntaxHighlighterColors.PARENTHS);
        ATTRIBUTES.put(RElementTypes.R_RPAR, RSyntaxHighlighterColors.PARENTHS);

        ATTRIBUTES.put(RElementTypes.R_LBRACE, RSyntaxHighlighterColors.BRACES);
        ATTRIBUTES.put(RElementTypes.R_RBRACE, RSyntaxHighlighterColors.BRACES);

        ATTRIBUTES.put(RElementTypes.R_LBRACKET, RSyntaxHighlighterColors.BRACKETS);
        ATTRIBUTES.put(RElementTypes.R_LDBRACKET, RSyntaxHighlighterColors.BRACKETS);
        ATTRIBUTES.put(RElementTypes.R_RBRACKET, RSyntaxHighlighterColors.BRACKETS);
        ATTRIBUTES.put(RElementTypes.R_RDBRACKET, RSyntaxHighlighterColors.BRACKETS);

        ATTRIBUTES.put(RElementTypes.R_COMMA, RSyntaxHighlighterColors.COMMA);
        ATTRIBUTES.put(RElementTypes.R_SEMI, RSyntaxHighlighterColors.SEMICOLON);

        ATTRIBUTES.put(RParserDefinition.END_OF_LINE_COMMENT, RSyntaxHighlighterColors.LINE_COMMENT);

        ATTRIBUTES.put(RParserDefinition.BAD_CHARACTER, RSyntaxHighlighterColors.BAD_CHARACTER);
    }

    @Override
    @NotNull
    public Lexer getHighlightingLexer() {
        return new RLexer();
    }


    @Override
    @NotNull
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return pack(ATTRIBUTES.get(tokenType));
    }
}
