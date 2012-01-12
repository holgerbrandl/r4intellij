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
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.LiteralTextEscaper;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.PsiReference;
import com.r4intellij.psi.RStringLiteral;
import org.jetbrains.annotations.NotNull;


/**
 * @author gregsh
 * @author brandl
 */
public abstract class RStringImpl extends RCompositeElementImpl implements RStringLiteral, PsiLanguageInjectionHost {

    public RStringImpl(ASTNode node) {
        super(node);
    }


    @Override
    public PsiReference getReference() {
        return null;
        // copied from bnf example
//      if (!(getParent() instanceof BnfAttrValue)) return null;
//      return new BnfReferenceImpl<BnfStringLiteralExpression>(this, TextRange.from(1, getTextLength() - 2)) {
//          @Override
//          public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
//              return getString().replace(RElementFactory.createLeafFromText(getProject(), '\"' + newElementName + '\"'));
//          }
//      };
    }

    @Override
    public boolean isValidHost() {
        return true;
    }

    @Override
    public RStringImpl updateText(@NotNull final String text) {
        final RStringImpl expression = RElementFactory.createExpressionFromText(getProject(), text);
        assert expression instanceof RStringImpl : text + "-->" + expression;
        return (RStringImpl) this.replace(expression);
    }

//  @Override
//  public RStringImpl updateText(@NotNull final String text) {
//    final BnfExpression expression = RElementFactory.createExpressionFromText(getProject(), text);
//    assert expression instanceof RStringImpl : text + "-->" + expression;
//    return (RStringImpl)this.replace(expression);
//  }

    @NotNull
    @Override
    public LiteralTextEscaper<? extends PsiLanguageInjectionHost> createLiteralTextEscaper() {
        return new RStringLiteralEscaper(this);
    }
}
