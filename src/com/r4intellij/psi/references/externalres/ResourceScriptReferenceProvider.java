/*
 * Copyright 2000-2009 JetBrains s.r.o.
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
package com.r4intellij.psi.references.externalres;

import com.intellij.openapi.paths.WebReference;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceProvider;
import com.intellij.psi.SyntheticElement;
import com.intellij.psi.impl.FakePsiElement;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

/**
 * Allows to inject references into resource script source URLs. E.g. devtools::source_url, or base::source
 * This allows to intercept "Open in Browser" action and redirect to library cache of web resources
 *
 * @author Holger Brandl
 */
public class ResourceScriptReferenceProvider extends PsiReferenceProvider {


    @Override
    @NotNull
    public PsiReference[] getReferencesByElement(@NotNull PsiElement element, @NotNull final ProcessingContext context) {
        WebReference reference = new WebReference(element) {
            @Override
            public PsiElement resolve() {
                return new WebRefInterceptorElement();
            }


            class WebRefInterceptorElement extends FakePsiElement implements SyntheticElement {
                @Override
                public PsiElement getParent() {
                    return myElement;
                }


                @Override
                public void navigate(boolean requestFocus) {
                    // todo redirect into cached library copy of resource file
//                    BrowserUtil.browse(getUrl());
                    Toolkit.getDefaultToolkit().beep();

                }


                @Override
                public String getPresentableText() {
                    return getUrl();
                }


                @Override
                public String getName() {
                    return getUrl();
                }


                @Override
                public TextRange getTextRange() {
                    final TextRange rangeInElement = getRangeInElement();
                    final TextRange elementRange = myElement.getTextRange();
                    return elementRange != null ? rangeInElement.shiftRight(elementRange.getStartOffset()) : rangeInElement;
                }
            }
        };
//        PsiReference reference = new PsiReferenceBase<String>() {};
        return new PsiReference[]{reference};
    }

}
