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
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RVariable;
import org.jetbrains.annotations.NotNull;


/**
 * Created by IntelliJ IDEA.
 * User: gregory
 * Date: 14.07.11
 * Time: 19:17
 */
public abstract class AbstractRFunCall extends RNamedElementImpl implements RVariable {

    public AbstractRFunCall(ASTNode node) {
        super(node);
    }

    @Override
    @NotNull
    public PsiElement getId() {
        return PsiTreeUtil.getChildOfType(this, RVariable.class);
//        ASTNode child = getNode().findChildByType(R_SYMBOL);
//        return child == null? null : child.getPsi();
    }
}
