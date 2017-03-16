package com.r4intellij;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.search.ProjectAndLibrariesScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Predicate;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.stubs.RAssignmentNameIndex;
import com.r4intellij.typing.ArgumentMatcher;
import com.r4intellij.typing.ArgumentsMatchResult;
import com.r4intellij.typing.MatchingException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class RPsiUtils {

    private static final Logger LOG = Logger.getInstance(RPsiUtils.class);

    public static final int MINUTE = 60 * 1000;


    public static List<RExpression> getParametersExpressions(List<RParameter> parameters) {
        List<RExpression> parametersExpressions = new ArrayList<RExpression>();
        for (RParameter parameter : parameters) {
            parametersExpressions.add(parameter.getExpression());
        }
        return parametersExpressions;
    }


    @Nullable
    public static RAssignmentStatement getAssignmentStatement(@NotNull final RParameter parameter) {
        RFunctionExpression functionExpression = getFunction(parameter);
        if (functionExpression == null) {
            return null;
        }
        PsiElement assignmentStatement = functionExpression.getParent();
        if (assignmentStatement == null || !(assignmentStatement instanceof RAssignmentStatement)) {
            return null;
        }
        return (RAssignmentStatement) assignmentStatement;
    }


    @Nullable
    public static RFunctionExpression getFunction(RParameter parameter) {
        //TODO: check some conditions when we should stop
        return PsiTreeUtil.getParentOfType(parameter, RFunctionExpression.class);
    }


    /**
     * Returns true if <code>element</code> is the LHS of named argument in a function call.
     */
    public static boolean isNamedArgument(RReferenceExpression element) {
        PsiElement parent = element.getParent();
        if (parent == null || !(parent instanceof RAssignmentStatement)) {
            return false;
        }
        PsiElement argumentList = parent.getParent();
        return argumentList != null &&
                argumentList instanceof RArgumentList &&
                ((RAssignmentStatement) parent).getAssignee() == element;
    }


    /**
     * Returns true if <code>element</code> is the LHS of named argument in a function call.
     */
    public static boolean isVarDeclaration(RReferenceExpression element) {
        PsiElement parent = element.getParent();

        if (parent == null || !(parent instanceof RAssignmentStatement)) {
            return false;
        }

        return ((RAssignmentStatement) parent).getAssignee().equals(element);
    }


    @Nullable
    public static RFunctionExpression getFunction(@NotNull final RCallExpression callExpression) {
        RExpression expression = callExpression.getExpression();
        if (expression instanceof RReferenceExpression) {
            return getFunctionFromReference(expression.getReference());
        }
        return null;
    }


    @Nullable
    private static RFunctionExpression getFunctionFromReference(PsiReference reference) {
        if (reference == null) {
            return null;
        }
        PsiElement functionDef = reference.resolve();
        if (functionDef == null) {
            return null;
        }
        if (functionDef instanceof RAssignmentStatement) {
            return PsiTreeUtil.getChildOfType(functionDef, RFunctionExpression.class);
        }
        PsiElement assignmentStatement = functionDef.getParent();
        return PsiTreeUtil.getChildOfType(assignmentStatement, RFunctionExpression.class);
    }


    @Nullable
    public static RFunctionExpression getFunction(@NotNull final ROperatorExpression binaryExpression) {
        ROperator operator = PsiTreeUtil.getChildOfType(binaryExpression, ROperator.class);
        if (operator != null) {
            return getFunctionFromReference(operator.getReference());
        }
        return null;
    }


    public static boolean containsTripleDot(List<RParameter> formalArguments) {
        for (RParameter parameter : formalArguments) {
            if (parameter.getText().equals("...")) {
                return true;
            }
        }
        return false;
    }


    public static RAssignmentStatement getAssignmentStatement(@NotNull final RFunctionExpression expression) {
        PsiElement assignmentStatement = expression.getParent();
        if (assignmentStatement != null && assignmentStatement instanceof RAssignmentStatement) {
            return (RAssignmentStatement) assignmentStatement;
        }
        return null;
    }


    @NotNull
    public static <T extends RPsiElement> T[] getAllChildrenOfType(@NotNull PsiElement element, @NotNull Class<T> aClass) {
        List<T> result = new SmartList<T>();
        for (PsiElement child : element.getChildren()) {
            if (aClass.isInstance(child)) {
                //noinspection unchecked
                result.add((T) child);
            } else {
                ContainerUtil.addAll(result, getAllChildrenOfType(child, aClass));
            }
        }
        return ArrayUtil.toObjectArray(result, aClass);
    }


    public static boolean isReturn(RCallExpression expression) {
        return expression.getText().startsWith("return");
    }


    public static RCallExpression findCall(Project project, String functionName, Predicate<RCallExpression> predicate) {
        ProjectAndLibrariesScope scope = new ProjectAndLibrariesScope(project);
        Collection<RAssignmentStatement> possibleDefinitions =
                RAssignmentNameIndex.find(functionName, project, scope);
        RAssignmentStatement functionDefinition = null;
        for (RAssignmentStatement assignment : possibleDefinitions) {
            if (assignment.getAssignedValue() instanceof RFunctionExpression) {
                functionDefinition = assignment;
                break;
            }
        }
        if (functionDefinition == null) {
            return null;
        }
        for (PsiReference reference : ReferencesSearch.search(functionDefinition, scope)) {
            PsiElement referenceFrom = reference.getElement();
            PsiElement parent = referenceFrom.getParent();
            if (parent == null || !RCallExpression.class.isInstance(parent)) {
                continue;
            }
            RCallExpression call = (RCallExpression) parent;
            if (predicate.apply(call)) {
                return call;
            }
        }
        return null;
    }


    public static RExpression findParameterValue(String param, RCallExpression callExpression) {
        return findParameterValues(callExpression, param).get(param);
    }


    public static Map<String, RExpression> findParameterValues(RCallExpression callExpression, String... params) {


        ArgumentsMatchResult matchResult;

        try {
            List<RExpression> arguments = callExpression.getArgumentList().getExpressionList();
            matchResult = new ArgumentMatcher(callExpression).matchArgs(arguments);
        } catch (MatchingException e) {
            return Collections.emptyMap();
        }

        Map<String, RExpression> result = new HashMap<String, RExpression>();
        for (Map.Entry<RExpression, RParameter> entry : matchResult.matchedParams.entrySet()) {
            String parameterName = entry.getValue().getName();
            RExpression expression = entry.getKey();
            if (expression instanceof RAssignmentStatement) {
                expression = (RExpression) ((RAssignmentStatement) expression).getAssignedValue();
            }
            for (String param : params) {
                if (param != null && param.equals(parameterName)) {
                    result.put(param, expression);
                }
            }
        }
        return result;
    }


    public static boolean isNamespacePrefix(@NotNull RReferenceExpression element) {
        PsiElement sibling = element.getNextSibling();
        return sibling != null && (
                sibling.getNode().getElementType() == RElementTypes.R_DOUBLECOLON ||
                        sibling.getNode().getElementType() == RElementTypes.R_TRIPLECOLON
        );
    }


    public static boolean isReturnValue(PsiElement o) {
        RFunctionExpression funExpr = PsiTreeUtil.getParentOfType(o, RFunctionExpression.class);

        if (funExpr == null) return false; // not part of a function def


        if (o instanceof RCallExpression) {
            Objects.equals(((RCallExpression) o).getName(), "return");
        }

        // the recursion is needed to resolve final blocks
//        return PsiTreeUtil.nextVisibleLeaf(o) == null && isReturnValue(o.getParent());
        return PsiTreeUtil.getNextSiblingOfType(o, RExpression.class) == null &&
                (o.getParent() instanceof RFunctionExpression || isReturnValue(o.getParent()));
    }


    public static java.util.function.Predicate<ResolveResult> createForwardRefPredicate(PsiElement element) {
        return resolveResult -> isForwardReference(resolveResult.getElement(), element);
    }


    // should go into RPsiUtil
    public static boolean isForwardReference(PsiElement resolvant, PsiElement element) {
        if (!resolvant.getContainingFile().equals(element.getContainingFile())) {
            return false;
        }

        boolean isSelfReference = (resolvant instanceof RAssignmentStatement) &&
                ((RAssignmentStatement) resolvant).getAssignee().equals(element);

        return !isSelfReference && (PsiTreeUtil.isAncestor(resolvant, element, true) ||
                resolvant.getTextOffset() > element.getTextOffset());
//        return  resolvant.getTextOffset() +1 < element.getTextOffset() ;
    }
}
