package com.r4intellij.documentation;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import com.r4intellij.RHelp;
import com.r4intellij.RPsiUtils;
import com.r4intellij.psi.api.RFunctionExpression;
import org.jetbrains.annotations.Nullable;


public class RDocumentationProvider extends AbstractDocumentationProvider {

    @Nullable
    @Override
    public String generateDoc(PsiElement element, @Nullable PsiElement element1) {
        for (PsiElement el = element.getFirstChild(); el != null; el = el.getNextSibling()) {
            if (el instanceof RFunctionExpression) {
                final String docString = ((RFunctionExpression) el).getDocStringValue();
                if (docString != null) {
                    return RDocumentationUtils.getFormattedString(docString);
                }
                break;
            }
        }
        final String helpText = RPsiUtils.getHelpForFunction(element, null);
        if (helpText == null) {
            return null;
        } else {
            return RDocumentationUtils.getFormattedString(new RHelp(helpText));
        }
    }
}
