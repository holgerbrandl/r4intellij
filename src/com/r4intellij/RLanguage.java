/*
 * Copyright 2009 Holger Brandl
 * File: BashLanguage.java, Class: BashLanguage
 * Last modified: 2009-12-04
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.r4intellij;

import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.SingleLazyInstanceSyntaxHighlighterFactory;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.r4intellij.editor.highlighting.RSyntaxHighlighter;
import org.jetbrains.annotations.NotNull;


/**
 * Date: 22.03.2009
 * Time: 11:12:46
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
