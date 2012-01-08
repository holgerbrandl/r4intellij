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
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.psi.RNamedElement;
import com.r4intellij.psi.RTypes;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;


public abstract class RNamedElementImpl extends RCompositeElementImpl implements RNamedElement {

    private volatile String myCachedName;

    public RNamedElementImpl(ASTNode node) {
        super(node);
    }

    @Override
    public void subtreeChanged() {
        super.subtreeChanged();
        myCachedName = null;
    }

    @Override
    public String getName() {
        if (myCachedName == null) {
            myCachedName = getId().getText();
        }
        return myCachedName;
    }

    @Override
    public PsiElement setName(@NonNls @NotNull String s) throws IncorrectOperationException {
        getId().replace(RElementFactory.createLeafFromText(getProject(), s));
        return this;
    }

    @Override
    public int getTextOffset() {
        return getId().getTextOffset();
    }

    @NotNull
    @Override
    public SearchScope getUseScope() {
        // todo change scope to project for function-renamings and local variables
        return new LocalSearchScope(getContainingFile());
    }

    @Override
    public Icon getIcon(int flags) {
//        if (this instanceof RRule) {
//            final Icon base = hasModifier((RRule) this, "external") ? RIcons.EXTERNAL_RULE : RIcons.RULE;
//            final Icon visibility = hasModifier((RRule) this, "private") ? PlatformIcons.PRIVATE_ICON : PlatformIcons.PUBLIC_ICON;
//            final RowIcon row = new RowIcon(2);
//            row.setIcon(base, 0);
//            row.setIcon(visibility, 1);
//            return row;
//        } else if (this instanceof RAttr) {
//            return RIcons.ATTRIBUTE;
//        }
        return super.getIcon(flags);
    }

    @NotNull
    public PsiElement getId() {
        ASTNode child = getNode().findChildByType(RTypes.R_SYMBOL);
        return child == null ? null : child.getPsi();
    }

    @Override
    public PsiElement getNameIdentifier() {
        return getId();
    }

//    public static boolean hasModifier(RRule rule, String modifier) {
//        for (RModifier o : rule.getModifierList()) {
//            if (modifier.equals(o.getText())) return true;
//        }
//        return false;
//    }

    @Override
    public String toString() {
        return super.toString() + ":" + getName();
    }
}
