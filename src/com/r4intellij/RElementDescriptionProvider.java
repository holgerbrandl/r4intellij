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
package com.r4intellij;

import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.ElementDescriptionProvider;
import com.intellij.psi.PsiElement;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import com.r4intellij.psi.RNamedElement;
import com.r4intellij.psi.RVariable;
import org.jetbrains.annotations.NotNull;


public class RElementDescriptionProvider implements ElementDescriptionProvider {

    @Override
    public String getElementDescription(@NotNull PsiElement psiElement, @NotNull ElementDescriptionLocation location) {
        if (location == UsageViewNodeTextLocation.INSTANCE && psiElement instanceof RNamedElement) {
            return getElementDescription(psiElement, UsageViewTypeLocation.INSTANCE) + " " +
                    "'" + getElementDescription(psiElement, UsageViewShortNameLocation.INSTANCE) + "'";
        }
        if (psiElement instanceof RVariable) {
            if (location == UsageViewTypeLocation.INSTANCE) {
                return "Variable";
            }
            if (location == UsageViewShortNameLocation.INSTANCE || location == UsageViewLongNameLocation.INSTANCE) {
                return ((RVariable) psiElement).getName();
            }
        }
//    else if (psiElement instanceof BnfAttr) {
//      if (location == UsageViewTypeLocation.INSTANCE) {
//        final BnfRule rule = PsiTreeUtil.getParentOfType(psiElement, BnfRule.class);
//        return (rule == null ? "Grammar " : "Rule ") + "Attribute";
//      }
//      if (location == UsageViewShortNameLocation.INSTANCE || location == UsageViewLongNameLocation.INSTANCE) {
//        return ((BnfAttr)psiElement).getId().getText();
//      }
//    }
        return null;
    }
}
