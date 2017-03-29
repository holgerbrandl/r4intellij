

package com.r4intellij.editor;

import com.google.common.collect.Lists;
import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionUtil;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.patterns.PsiJavaPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.cache.impl.id.IdTableBuilding;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.packages.RIndexCache;
import com.r4intellij.packages.RPackage;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;


/**
 * This would be our hook into any special handling for code completion.
 */
public class RCompletionContributor extends CompletionContributor {

    public static ArrayList<String> PACKAGE_IMPORT_METHODS = Lists.newArrayList("require", "library", "load_pack");


    public RCompletionContributor() {
        // also allow for within string completion
        // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/206114249-How-to-complete-string-literal-expressions-

        // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/206756005-Is-There-A-Standard-CompletionContributor-Which-Provides-Path-File-Completion
//        extend(PlatformPatterns.psiElement().inside(PsiJavaPatterns.literalExpression()));
    }

    //    public static final String test = new File("");


    @Override
    public void fillCompletionVariants(@NotNull final CompletionParameters parameters, @NotNull final CompletionResultSet result) {
//        if (parameters.getCompletionType() == CompletionType.BASIC && shouldPerformWordCompletion(parameters)) {
        addWordCompletionVariants(result, parameters, Collections.<String>emptySet());
        PlatformPatterns.psiElement().inside(PsiJavaPatterns.literalExpression());

//        }
    }


    // see http://www.jetbrains.org/intellij/sdk/docs/tutorials/custom_language_support/completion_contributor.html
    private static void addWordCompletionVariants(CompletionResultSet result, CompletionParameters parameters, Set<String> excludes) {

        PsiElement insertedElement = parameters.getPosition();
        RCallExpression pp = PsiTreeUtil.getContextOfType(insertedElement, RCallExpression.class);


        boolean isPackageContext = pp != null && PACKAGE_IMPORT_METHODS.contains(pp.getExpression().getText());

        if (isPackageContext) {         // .. auto-completion for require and libary

//            List<RepoPackage> allPackages = LocalRUtil.getPckgNameVersionMap();
            Set<RPackage> allPackages = RIndexCache.getInstance().getPackages();

            // TODO add completion for not-yet-installed packages

            final CompletionResultSet plainResultSet = result.
                    withPrefixMatcher(CompletionUtil.findAlphanumericPrefix(parameters));

            for (RPackage p : allPackages) {
                plainResultSet.addElement(LookupElementBuilder.create(p.getName()).
                        withTypeText(p.getTitle()));
            }
        } else {
            addWordFromDocument(result, parameters, excludes);

            // Also add function names of loaded packages
            final CompletionResultSet plainResultSet = result.
                    withPrefixMatcher(CompletionUtil.findAlphanumericPrefix(parameters));


            PsiFile containingFile = insertedElement.getContainingFile();
            if (containingFile instanceof RFile) {
                List<String> importedPackages = ((RFile) containingFile).getImportedPackages(insertedElement);

                for (String pckg : importedPackages) {
                    RPackage byName = RIndexCache.getInstance().getByName(pckg);
                    if (byName != null) {
//                        plainResultSet.addElement(LookupElementBuilder.create(p.getName()).withTypeText(p.getTitle()));
                        byName.getFunctionNames().forEach(funName -> plainResultSet.addElement(LookupElementBuilder.create(funName)));
                    }
                }

            }
//            if(parameters.isExtendedCompletion()){

//            }

//            RFile rFile = PsiTreeUtil.getContextOfType(insertedElement, RFile.class);
        }
    }


    private static void addWordFromDocument(CompletionResultSet result, CompletionParameters parameters, Set<String> excludes) {
        Set<String> realExcludes = new HashSet<String>(excludes);
        for (String exclude : excludes) {
            String[] words = exclude.split("[ \\.-]");
            if (words.length > 0 && StringUtil.isNotEmpty(words[0])) {
                realExcludes.add(words[0]);
            }
        }

        int startOffset = parameters.getOffset();
        PsiElement insertedElement = parameters.getPosition();

//        RCallExpression pp = PsiTreeUtil.getContextOfType(insertedElement, RCallExpression.class);

        final CompletionResultSet plainResultSet = result.
                withPrefixMatcher(CompletionUtil.findAlphanumericPrefix(parameters));

        for (final String word : new HashSet<String>(getAllWords(insertedElement, startOffset))) {
            if (!realExcludes.contains(word)) {
                plainResultSet.addElement(LookupElementBuilder.create(word));
            }
        }
    }


    private static Set<String> getAllWords(final PsiElement context, final int offset) {
        final Set<String> words = new LinkedHashSet<String>();
        if (StringUtil.isEmpty(CompletionUtil.findJavaIdentifierPrefix(context, offset))) {
            return words;
        }

        final CharSequence chars = context.getContainingFile().getViewProvider().getContents(); // ??
        IdTableBuilding.scanWords(new IdTableBuilding.ScanWordProcessor() {
            public void run(final CharSequence chars, @Nullable char[] charsArray, final int start, final int end) {
                if (start > offset || offset > end) {
                    words.add(chars.subSequence(start, end).toString());
                }
            }
        }, chars, 0, chars.length());
        return words;
    }


}
