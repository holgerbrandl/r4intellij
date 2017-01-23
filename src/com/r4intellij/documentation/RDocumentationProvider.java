package com.r4intellij.documentation;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.psi.PsiElement;
import com.r4intellij.RHelp;
import com.r4intellij.RPsiUtils;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RFunctionExpression;
import org.jetbrains.annotations.Nullable;

/**
 * For local function definitions provide doc string documentation (using docstring)
 * For library functions use R help.
 */
public class RDocumentationProvider extends AbstractDocumentationProvider {

    @Nullable
    @Override
    public String generateDoc(PsiElement element, @Nullable PsiElement element1) {
        // if function call

        // process locally defined function definitions
        for (PsiElement el : element.getChildren()) {
            if (el instanceof RFunctionExpression) {
                final String docString = ((RFunctionExpression) el).getDocStringValue();
                return docString != null ? docString : "No doc-string for locally defined function";
            }
        }

        // check if it's a library function and return help if it is


        if (element1 != null && element1.getParent() != null && element1.getParent().getParent() instanceof RCallExpression) {
            String elementText = element1.getText();

            RPackageService.getInstance().getContainingPackages(elementText);


            // todo try to detect package
            String helpText = RPsiUtils.getHelpForFunction(elementText, null);

            if (helpText == null) {
                return null;
            } else {
                return RDocumentationUtils.getFormattedString(new RHelp(helpText));
            }
        }

        //


        return null;
    }
}
