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
package com.r4intellij.quickfixes;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.psi.PsiElement;
import com.r4intellij.psi.RFuncall;
import com.r4intellij.psi.RVariable;
import com.r4intellij.psi.impl.RPsiUtils;
import com.r4intellij.rinstallcache.PackageCache;
import com.r4intellij.rinstallcache.PackageCacheService;
import com.r4intellij.rinstallcache.RPackage;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;


/**
 * @author gregsh
 */
public class FunctionNameAnnotator implements Annotator {

    @Override
    public void annotate(@NotNull PsiElement psiElement, @NotNull AnnotationHolder annotationHolder) {
        if (psiElement instanceof RFuncall) {
            RFuncall funcall = (RFuncall) psiElement;
            RVariable funVar = ((RFuncall) psiElement).getVariable();

            // is is a locally defined function?
            if (funVar.getReference() != funVar) {
                PackageCacheService cacheService = ServiceManager.getService(PackageCacheService.class);
                PackageCache cache = cacheService.getCache();
                if (cache != null) {
                    List<RPackage> funPackages = cache.getPackagesOfFunction(funVar.getText());
                    List<String> funPackageNames = new ArrayList<String>();
                    for (RPackage funPackage : funPackages) {
                        funPackageNames.add(funPackage.getName());
                    }

                    // check if there's an import statement for any of them
                    List<RFuncall> libraryStatements = RPsiUtils.collectLibraryStatements(psiElement.getContainingFile());

                    // check whether the import list contains any of the packages
                    boolean isImported = false;
                    for (RFuncall libraryStatement : libraryStatements) {
                        String importedPackage = libraryStatement.getFormlist().getFormList().get(0).getText();
                        if (funPackageNames.contains(importedPackage)) {
                            isImported = true;
                            break;
                        }
                    }

                    if (isImported)
                        return;

                    // no overlap --> highlight as error and suggest to import one!
                    annotationHolder.createErrorAnnotation(funVar, "Unresolved reference");


                    // todo why not GrammarUtil.isExternalReference(psiElement)
                }

            }
        }
//
//    PsiElement parent = psiElement.getParent();
//    if (parent instanceof BnfRule && ((BnfRule)parent).getId() == psiElement) {
//      annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.RULE);
//    }
//    else if (parent instanceof BnfAttr && ((BnfAttr)parent).getId() == psiElement) {
//      annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.ATTRIBUTE);
//    }
//    else if (parent instanceof BnfModifier) {
//      annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.KEYWORD);
//    }
//    else if (psiElement instanceof BnfRefOrTokenImpl) {
//      if (parent instanceof BnfAttrValue) {
//        String text = psiElement.getText();
//        if (text.equals("true") || text.equals("false")) {
//          annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.KEYWORD);
//          return;
//        }
//      }
//      PsiReference reference = psiElement.getReference();
//      Object resolve = reference == null ? null : reference.resolve();
//      if (resolve instanceof BnfRule) {
//        annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.RULE);
//      }
//      else if (resolve instanceof BnfAttr) {
//        annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.ATTRIBUTE);
//      }
//      else if (resolve == null && parent instanceof BnfAttrValue) {
//        annotationHolder.createErrorAnnotation(psiElement, "Unresolved reference");
//      }
//      else if (resolve == null && !(parent instanceof BnfModifier)) {
//        if (GrammarUtil.isExternalReference(psiElement)) {
//          annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.EXTERNAL);
//        }
//        else {
//          annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(BnfSyntaxHighlighter.TOKEN);
//        }
//      }
//    }
//    else if (psiElement instanceof BnfStringLiteralExpression && parent instanceof BnfAttrValue) {
//      final String attrName = ((PsiNamedElement)parent.getParent()).getName();
//      if (Arrays.asList("extends", "implements", "recoverUntil").contains(attrName)
//          && !psiElement.getText().contains(".")) {
//        PsiReference reference = psiElement.getReference();
//        Object resolve = reference == null ? null : reference.resolve();
//        if (resolve instanceof BnfRule) {
//          annotationHolder.createInfoAnnotation(reference.getRangeInElement().shiftRight(psiElement.getTextRange().getStartOffset()), null)
//            .setTextAttributes(BnfSyntaxHighlighter.RULE);
//        }
//        else if (resolve == null) {
//          annotationHolder.createErrorAnnotation(psiElement, "Unresolved reference");
//        }
//      }
//    }
    }
}
