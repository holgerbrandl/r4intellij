package com.r4intellij;

import com.google.common.collect.Lists;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.ProjectAndLibrariesScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Predicate;
import com.r4intellij.interpreter.RInterpreterService;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.stubs.RAssignmentNameIndex;
import com.r4intellij.typing.MatchingException;
import com.r4intellij.typing.RTypeChecker;
import com.r4intellij.typing.RTypeProvider;
import com.r4intellij.typing.types.RFunctionType;
import com.r4intellij.typing.types.RType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
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


    public static boolean isNamedArgument(RReferenceExpression element) {
        PsiElement parent = element.getParent();
        if (parent == null || !(parent instanceof RAssignmentStatement)) {
            return false;
        }
        PsiElement argumentList = parent.getParent();
        return argumentList != null && argumentList instanceof RArgumentList;
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


    /**
     * If packageName parameter equals null we do not load package
     */
    @Nullable
    public static String getHelpForFunction(@NotNull final String assignee, @Nullable final String packageName) {
        final String helpHelper = packageName != null ? "r-help.r" : "r-help-without-package.r";
        final File file = RHelpersLocator.getHelperFile(helpHelper);

        final String path = RInterpreterService.getInstance().getInterpreterPath();

        final String helperPath = file.getAbsolutePath();


        try {

            if (assignee.isEmpty()) {
                return null;
            }

            final ArrayList<String> arguments = Lists.newArrayList(path, "--slave", "-f ", helperPath, " --args ");
            if (packageName != null) {
                arguments.add(packageName);
            }
            arguments.add(assignee);

            final GeneralCommandLine commandLine = new GeneralCommandLine(arguments);

            LOG.info("getting help for '" + assignee + "' with: " + commandLine.getCommandLineString());

            final CapturingProcessHandler processHandler = new CapturingProcessHandler(commandLine);
            final ProcessOutput output = processHandler.runProcess(MINUTE * 5);

            String stdout = output.getStdout();

            if (stdout.startsWith("No documentation")) {
                return null;
            }
            return stdout;
        } catch (ExecutionException e) {
            LOG.error(e);
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
        RFunctionExpression function = RPsiUtils.getFunction(callExpression);
        final RFunctionType functionType;
        if (function != null) {
            functionType = new RFunctionType(function);
        } else {
            RType type = RTypeProvider.getType(callExpression.getExpression());
            if (!RFunctionType.class.isInstance(type)) {
                return Collections.emptyMap();
            }
            functionType = (RFunctionType) type;
        }
        Map<RExpression, RParameter> matchedParams = new HashMap<RExpression, RParameter>();
        List<RExpression> matchedByTripleDot = new ArrayList<RExpression>();
        try {
            RTypeChecker.matchArgs(callExpression.getArgumentList().getExpressionList(), matchedParams, matchedByTripleDot, functionType);
        } catch (MatchingException e) {
            return Collections.emptyMap();
        }
        Map<String, RExpression> result = new HashMap<String, RExpression>();
        for (Map.Entry<RExpression, RParameter> entry : matchedParams.entrySet()) {
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
}
