/*
 * Copyright 2000-2011 JetBrains s.r.o.
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
package com.r4intellij.apiplayground;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;

/**
 * Note: nice way to provide hover and popup documentation for arbitrary psi-elements
 *
 * @author Holger Brandl
 *         <p>
 */
//         <documentationProvider implementation="com.r4intellij.apiplayground.WebResourceDocumentationProvider"/>

public class WebResourceDocumentationProvider extends AbstractDocumentationProvider {
    @Override
    public String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        if (element.getClass().getSimpleName().equals("WebRefInterceptorElement")) {
//    if (element instanceof RStringLiteralExpression) {
//      return IdeBundle.message("open.url.in.browser.tooltip");
            return "Hello R";
        }
        return null;
    }
}
