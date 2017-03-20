package com.r4intellij.inspections;

import com.google.common.base.Joiner;
import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RPsiUtils;
import com.r4intellij.editor.RCompletionContributor;
import com.r4intellij.intentions.ImportLibraryFix;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.references.RReferenceImpl;
import com.r4intellij.psi.references.ResolveResultWrapper;
import com.r4intellij.settings.RCodeInsightSettings;
import com.r4intellij.typing.*;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.r4intellij.psi.RPsiImplUtil.BUILTIN_CONSTANTS;

public class UnresolvedReferenceInspection extends RInspection {


    public static String UNRESOLVED_MSG = "Unresolved reference";

    @Nls
    @NotNull
    @Override
    public String getDisplayName() {
        return "Unresolved reference";
    }


    @NotNull
    @Override
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
        return new ReferenceVisitor(holder);
    }


    private class ReferenceVisitor extends RVisitor {

        private final ProblemsHolder myProblemHolder;


        public ReferenceVisitor(@NotNull ProblemsHolder holder) {
            myProblemHolder = holder;
        }


        @Override
        public void visitOperator(@NotNull ROperator element) {
            if (!element.getText().startsWith("%")) return;

            ResolveResultWrapper resultWrapper = element.getReference().resolve(false);
            handleResolveResult(element, resultWrapper);
        }


        @Override
        public void visitReferenceExpression(@NotNull RReferenceExpression element) {
            // how does/should it work:
            // exclude (ie. white-list) certain situations to show up as unresolved, but generally use
            // Ref.resolve() to check for resolvability


            if (RPsiUtils.isNamedArgument(element)) {
                return;
            }

            if (BUILTIN_CONSTANTS.contains(element.getFirstChild().getNode().getElementType())) {
                return;
            }

            // don't tag initial declarations of variables as unresolvable
            if (RPsiUtils.isVarDeclaration(element)) {
                return;
            }

            // ignore function calls here because they are handled by the missing import inspection
//            if (element.getParent() instanceof RCallExpression) {
//                if (resolveInPackages((RCallExpression) element.getParent(), myProblemHolder)) {
//                    // we could find it in a package or locally so it's somehow resolvable but not yet imported
//                    return;
//                }
//            }


            RCallExpression callExpression = PsiTreeUtil.getParentOfType(element, RCallExpression.class);

//            // this is type-checker stuff and should be handled there ## todo remove code bits once test are running
//            // ignore named arguments in tripledot calls. e.g. myfun=function(a, ...) a; myfun(23, b=4)
//            if (callExpression != null) {
//                RFunctionExpression function = RPsiUtils.getFunction(callExpression);
//                if (function != null) {
//                    List<RParameter> list = function.getParameterList().getParameterList();
//                    if (RPsiUtils.containsTripleDot(list)) {
//                        return;
//                    }
//                }
//            }


            // prevent package names to show up as unresolved, because they are handled separately by
            // the missing package inspection
            if (callExpression != null &&
                    RCompletionContributor.PACKAGE_IMPORT_METHODS.contains(callExpression.getExpression().getName())) {
                return;
            }

            // prevent package names to show up as unresolved in packageName-calls, because they are handled separately by
            // the missing package inspection
            if (RPsiUtils.isNamespacePrefix(element)) return;


            // ignore symbols in formulae. Those are by design unquoted and not resolved statically
            if (PsiTreeUtil.getParentOfType(element, RUnaryTildeExpression.class, RTildeExpression.class) != null) {
                return;
            }

            // resolve normally
            RReferenceImpl reference = element.getReference();

            if (reference != null) {
                ResolveResultWrapper resolve = reference.resolve(true);

                handleResolveResult(element, resolve);
            }
        }


        private void handleResolveResult(@NotNull RPsiElement element, ResolveResultWrapper resolveWrapper) {
            PsiElement resolve = resolveWrapper.getBest();

            if (resolve == null) {
                // exclude white-listed argument positions containing unquoted variable names
                if (isInUnquotedContext(element)) return;


                // add import suggestions if available
                List<String> packageOptions = resolveWrapper.getImportOptions();
                if (!packageOptions.isEmpty()) {
                    List<LocalQuickFix> fixes = new ArrayList<>();
                    for (String funPackageName : packageOptions) {
                        fixes.add(new ImportLibraryFix(funPackageName));
                    }

                    String descriptionTemplate = missingImportMsg(element.getName(), packageOptions);
                    myProblemHolder.registerProblem(element, descriptionTemplate, fixes.toArray(new LocalQuickFix[0]));

                } else {
                    // we have no clue about the symbol, so flag it as unresolved
//                    myProblemHolder.registerProblem(element, UNRESOLVED_MSG, ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
                    myProblemHolder.registerProblem(element, UNRESOLVED_MSG, ProblemHighlightType.GENERIC_ERROR);
                }

            } else if (RPsiUtils.isForwardReference(resolve, element)) {
                myProblemHolder.registerProblem(element, "Forward reference", ProblemHighlightType.GENERIC_ERROR);
            }
        }
    }


    public static String missingImportMsg(String symbol, List<String> foundIn) {
        return "'" + symbol + "' has been detected in a package (" + Joiner.on(", ").join(foundIn) + ") which does not seem to be imported yet.";
    }


    private static boolean isInUnquotedContext(PsiElement element) {
        RCallExpression callExpression = PsiTreeUtil.getParentOfType(element, RCallExpression.class);

        if (callExpression == null) return false;

        ArgumentsMatchResult matchResult;

        try {
            matchResult = new ArgumentMatcher(callExpression).matchArgs(callExpression.getArgumentList());

        } catch (MatchingException | UnknownTypeException e) {
            // we failed to match the args, so we can not apply white-listing rules
            return false;
        }

        // ignore dots if call is pipe-target
        PipeInfo pipeInfo = matchResult.getMatcher().getPipeInfo();
        if (pipeInfo.isPipeTarget && element.getText().equals(".")) {
            return true;
        }

        List<UnquotedArgsRule> wlRules = RCodeInsightSettings.getInstance().getWhitelistModel();

        // find parameter of given `element`
        RParameter rParameter = matchResult.matchedParams.entrySet().stream().filter(pair -> {
//            pair.getKey()==element
            return PsiTreeUtil.isAncestor(pair.getKey(), element, false);
//            return commonContext != callExpression;
        }).map(Map.Entry::getValue).findFirst().orElse(null);

        if (rParameter != null) { // is it white-listed?
            // see com.r4intellij.parser.UnquotedVariablesTest.testCascadedCallAsNamedArg()
            boolean isWhiteListedArg = wlRules.stream().anyMatch(rule -> rule.matches(rParameter));

            return isWhiteListedArg || isInUnquotedContext(callExpression);
        }

        // if it's not a named parameter, is must be a triple dot match
        ArgumentsMatchResult finalMatchResult = matchResult;
        boolean isWhiteListedTD = wlRules.stream().anyMatch(rule ->
                rule.matchesTripleDot(finalMatchResult.getFunctionType().getFunctionExpression())
        );

        return isWhiteListedTD || isInUnquotedContext(callExpression);
    }


}
