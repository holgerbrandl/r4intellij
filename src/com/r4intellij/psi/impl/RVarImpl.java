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
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.psi.RVariable;


/**
 * Created by IntelliJ IDEA.
 * User: gregory
 * Date: 14.07.11
 * Time: 19:17
 */
public abstract class RVarImpl extends RNamedElementImpl implements RVariable {

    public RVarImpl(ASTNode node) {
        super(node);
    }

    @Override
    public PsiReference getReference() {
        return new RReferenceImpl<RVarImpl>(this, TextRange.from(0, getTextLength())) {
            @Override
            public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
                myElement.getId().replace(RElementFactory.createLeafFromText(getElement().getProject(), newElementName));
                return myElement;
            }
        };
    }
}
