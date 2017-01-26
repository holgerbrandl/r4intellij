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
package com.r4intellij.psi;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.psi.LiteralTextEscaper;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.r4intellij.RLanguage;
import com.r4intellij.psi.api.RStringLiteralExpression;
import org.jetbrains.annotations.NotNull;


/**
 * @author gregsh
 * @author brandl
 */
public abstract class RStringInjectHost extends ASTWrapperPsiElement implements RStringLiteralExpression, PsiLanguageInjectionHost {

    public RStringInjectHost(ASTNode node) {
        super(node);
    }


    @Override
    public boolean isValidHost() {
        return true;
    }


    /**
     * note: this method is called if incjected snippet is edited in split pane view.
     */
    @Override
    public RStringInjectHost updateText(@NotNull final String text) {
        final RStringLiteralExpression expression = createExpressionFromText(getProject(), text);
        assert expression instanceof RStringLiteralExpression : text + "-->" + expression;
        return (RStringInjectHost) this.replace(expression);
    }


    public static RStringInjectHost createExpressionFromText(Project project, String text) {
//        PsiFile fromText = PsiFileFactory.getInstance(project).createFileFromText("a.R", "\"" + text + "\";");
        PsiFile fromText = PsiFileFactory.getInstance(project).createFileFromText("a.R", RLanguage.getInstance(), text + ";");
        if ((fromText.getFirstChild()) != null) {
            return (RStringInjectHost) fromText.getFirstChild();
        }
        return null;
    }


    @NotNull
    @Override
    public LiteralTextEscaper<? extends PsiLanguageInjectionHost> createLiteralTextEscaper() {
        return new RStringLiteralEscaper(this);
    }
}
