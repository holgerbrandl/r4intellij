/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij;

import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.SingleLazyInstanceSyntaxHighlighterFactory;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.r4intellij.editor.highlighting.RSyntaxHighlighter;
import org.jetbrains.annotations.NotNull;


/**
 * The definition of the R language for our plugin.
 *
 * @author Holger Brandl
 */
public class RLanguage extends Language {

    public static final RLanguage INSTANCE = new RLanguage();

    public RLanguage() {
        super("R", "text/R");

        SyntaxHighlighterFactory.LANGUAGE_FACTORY.addExplicitExtension(this, new RHighlighterFactory());
    }

    private static class RHighlighterFactory extends SingleLazyInstanceSyntaxHighlighterFactory {

        @NotNull
        protected SyntaxHighlighter createHighlighter() {
            return new RSyntaxHighlighter();
        }
    }
}
