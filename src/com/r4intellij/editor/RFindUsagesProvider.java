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
package com.r4intellij.editor;

import com.intellij.lang.cacheBuilder.WordsScanner;
import com.intellij.lang.findUsages.FindUsagesProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.r4intellij.psi.RVariable;
import org.jetbrains.annotations.NotNull;


/**
 * Inspired by com.ansorgit.plugins.bash.editor.usages.BashFindUsagesProvider
 *
 * @author brandl
 */
public class RFindUsagesProvider implements FindUsagesProvider {

//    private WordsScanner wordsScanner;

    @Override
    public WordsScanner getWordsScanner() {
//        if (wordsScanner == null) {
//           TokenSet literals = TokenSet.create(RTypes.R_STRING_LITERAL);
//           TokenSet comments = TokenSet.create(RTypes.R_COMMENT);
////            TokenSet identifierTokenSet = TokenSet.orSet(keywords, TokenSet.create(INTERNAL_COMMAND));
//
//            TokenSet identifierTokenSet = TokenSet.create(RTypes.R_VARIABLE, RTypes.R_SYMBOL);
//
//
//            wordsScanner = new DefaultWordsScanner(new RLexer(), identifierTokenSet, comments, literals);
//        }
//
//        return wordsScanner;
        return null;
    }

    @Override
    public boolean canFindUsagesFor(@NotNull PsiElement psiElement) {
        return psiElement instanceof RVariable; //todo should be rather RNamed
    }

    @Override
    public String getHelpId(@NotNull PsiElement psiElement) {
        return null;
    }

    @NotNull
    @Override
    public String getType(@NotNull PsiElement element) {
        if (element instanceof RVariable) {
            return "Variable";
        }

        return "";
    }

    @NotNull
    @Override
    public String getDescriptiveName(@NotNull PsiElement element) {
        if (!canFindUsagesFor(element)) {
            return "";
        }

        String name = ((PsiNamedElement) element).getName();
        return name != null ? name : "";
    }

    @NotNull
    @Override
    public String getNodeText(@NotNull PsiElement element, boolean useFullName) {
        return getDescriptiveName(element);
    }
}
