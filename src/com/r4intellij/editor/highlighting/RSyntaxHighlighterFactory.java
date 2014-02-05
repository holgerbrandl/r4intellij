/*
 * Copyright 2011-2011 Gregory Shrago
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.r4intellij.editor.highlighting;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.r4intellij.lang.lexer.RLexer;
import com.r4intellij.psi.RTypes;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.psi.RTypes.*;


/**
 * @author brandl
 */
public class RSyntaxHighlighterFactory extends SyntaxHighlighterFactory {

    @NotNull
    @Override
    public SyntaxHighlighter getSyntaxHighlighter(Project project, VirtualFile virtualFile) {
        return new MyHighlighter();
    }


    static TokenSet keywords = TokenSet.create(R_ELSE, R_FOR, R_FUNCTION, R_IF, R_WHILE, R_BREAK, R_REPEAT, R_IN);


    private class MyHighlighter extends SyntaxHighlighterBase {

        @NotNull
        @Override
        public Lexer getHighlightingLexer() {
            return new RLexer();
        }


        @NotNull
        @Override
        public TextAttributesKey[] getTokenHighlights(IElementType iElementType) {
            if (iElementType == TokenType.BAD_CHARACTER) {
                return pack(DefaultLanguageHighlighterColors.INVALID_STRING_ESCAPE);
            } else if (iElementType == RTypes.R_COMMENT || iElementType == RTypes.R_SECTION_COMMENT) {
                return pack(DefaultLanguageHighlighterColors.LINE_COMMENT);
            } else if (iElementType == RTypes.R_STR_CONST) {
                return pack(DefaultLanguageHighlighterColors.STRING);
            } else if (iElementType == RTypes.R_NUM_CONST) {
                return pack(DefaultLanguageHighlighterColors.NUMBER);
            } else if (keywords.contains(iElementType)) {
                return pack(DefaultLanguageHighlighterColors.KEYWORD);
            }

            return EMPTY;
        }
    }
}
