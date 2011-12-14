/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang;

import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionUtil;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.cache.impl.id.IdTableBuilding;
import org.jetbrains.annotations.Nullable;

import java.util.*;


/**
 * This would be our hook into any special handling for code completion.
 */
public class RCompletionContributor extends CompletionContributor {

    @Override
    public void fillCompletionVariants(final CompletionParameters parameters, final CompletionResultSet result) {
//        if (parameters.getCompletionType() == CompletionType.BASIC && shouldPerformWordCompletion(parameters)) {
        addWordCompletionVariants(result, parameters, Collections.<String>emptySet());
//        }
    }

    public static void addWordCompletionVariants(CompletionResultSet result, CompletionParameters parameters, Set<String> excludes) {
        Set<String> realExcludes = new HashSet<String>(excludes);
        for (String exclude : excludes) {
            String[] words = exclude.split("[ \\.-]");
            if (words.length > 0 && StringUtil.isNotEmpty(words[0])) {
                realExcludes.add(words[0]);
            }
        }

        int startOffset = parameters.getOffset();
        PsiElement insertedElement = parameters.getPosition();
//        final CompletionResultSet javaResultSet = result.withPrefixMatcher(CompletionUtil.findJavaIdentifierPrefix(parameters));
        final CompletionResultSet plainResultSet = result.withPrefixMatcher(CompletionUtil.findAlphanumericPrefix(parameters));
        for (final String word : getAllWordsSorted(insertedElement, startOffset)) {
            if (!realExcludes.contains(word)) {
                final LookupElement item = LookupElementBuilder.create(word);
//                javaResultSet.addElement(item);
                plainResultSet.addElement(item);
            }
        }

        System.out.println(plainResultSet);
    }


    public static Set<String> getAllWords(final PsiElement context, final int offset) {
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


    public static List<String> getAllWordsSorted(final PsiElement context, final int offset) {
        final List<String> words = new ArrayList<String>();
//        if (StringUtil.isEmpty(CompletionUtil.findJavaIdentifierPrefix(context, offset))) {
//            return words;
//        }

        // first add the words until the cursor
        final CharSequence chars = context.getContainingFile().getViewProvider().getContents(); // ??
        IdTableBuilding.scanWords(new IdTableBuilding.ScanWordProcessor() {
            public void run(final CharSequence chars, @Nullable char[] charsArray, final int start, final int end) {
                if (start > offset || offset > end) {
                    words.add(chars.subSequence(start, end).toString());
                }
            }
        }, chars, 0, offset);

//        revert the list to make closeby ones to show up first
        Collections.reverse(words);

        IdTableBuilding.scanWords(new IdTableBuilding.ScanWordProcessor() {
            public void run(final CharSequence chars, @Nullable char[] charsArray, final int start, final int end) {
                if (start > offset || offset > end) {
                    words.add(chars.subSequence(start, end).toString());
                }
            }
        }, chars, offset, chars.length());

        return words;
    }


//    private static boolean shouldPerformWordCompletion(CompletionParameters parameters) {
//        final PsiElement insertedElement = parameters.getPosition();
//        final boolean dumb = DumbService.getInstance(insertedElement.getProject()).isDumb();
//        if (dumb) {
//            return true;
//        }
//
//        if (parameters.getInvocationCount() == 0) {
//            return false;
//        }
//
//
//
//        final PsiFile file = insertedElement.getContainingFile();
//        final CompletionData data = CompletionUtil.getCompletionDataByElement(insertedElement, file);
//        if (data != null && !(data instanceof SyntaxTableCompletionData)) {
//            Set<CompletionVariant> toAdd = new HashSet<CompletionVariant>();
//            data.addKeywordVariants(toAdd, insertedElement, file);
//            for (CompletionVariant completionVariant : toAdd) {
//                if (completionVariant.hasKeywordCompletions()) {
//                    return false;
//                }
//            }
//        }
//
//        final int startOffset = parameters.getOffset();
//
//        final PsiReference reference = file.findReferenceAt(startOffset);
//        if (reference != null) {
//            return false;
//        }
//
//        final PsiElement element = file.findElementAt(startOffset - 1);
//
//        ASTNode textContainer = element != null ? element.getNode() : null;
//        while (textContainer != null) {
//            final IElementType elementType = textContainer.getElementType();
//            if (LanguageWordCompletion.INSTANCE.isEnabledIn(elementType) || elementType == PlainTextTokenTypes.PLAIN_TEXT) {
//                return true;
//            }
//            textContainer = textContainer.getTreeParent();
//        }
//        return false;
//    }


}
