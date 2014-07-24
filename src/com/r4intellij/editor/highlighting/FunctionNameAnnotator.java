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
package com.r4intellij.editor.highlighting;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.psi.PsiElement;
import com.r4intellij.psi.RFuncall;
import com.r4intellij.psi.RVariable;
import org.jetbrains.annotations.NotNull;


/**
 * @author brandl
 * @author moon
 */
public class FunctionNameAnnotator implements Annotator {

    @Override
    public void annotate(@NotNull PsiElement psiElement, @NotNull AnnotationHolder annotationHolder) {

      if (psiElement instanceof RFuncall)
	  {
		annotationHolder.createInfoAnnotation(psiElement.getFirstChild(), null).setTextAttributes(RHighlighterColors.FUNCALL_ATTR_KEY);
      }
	  else if(psiElement instanceof RVariable)
	  {
		annotationHolder.createInfoAnnotation(psiElement, null).setTextAttributes(RHighlighterColors.VARIABLE_ATTR_KEY);
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
