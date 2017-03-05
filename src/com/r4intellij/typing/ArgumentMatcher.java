package com.r4intellij.typing;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RPsiUtils;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.types.RFunctionType;
import com.r4intellij.typing.types.RType;
import com.r4intellij.typing.types.RUnknownType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ArgumentMatcher {


    private RFunctionType functionType;

    private boolean firstArgInjected;


    public ArgumentMatcher(RFunctionType functionType) {
        this.functionType = functionType;
    }


    public ArgumentMatcher(PsiReference referenceToFunction) {
        PsiElement assignmentStatement = referenceToFunction.resolve();

        if (assignmentStatement != null && assignmentStatement instanceof RAssignmentStatement) {
            RAssignmentStatement assignment = (RAssignmentStatement) assignmentStatement;
            RPsiElement assignedValue = assignment.getAssignedValue();

            if (assignedValue != null && assignedValue instanceof RFunctionExpression) {
                RFunctionExpression function = (RFunctionExpression) assignedValue;

                //        RType type = RTypeProvider.getType(functionExpression);
                //        if (!RFunctionType.class.isInstance(type)) {
                //            return; // TODO: fix me properly
                //        }
                //        RFunctionType functionType = (RFunctionType) type;
                functionType = new RFunctionType(function);

                // todo re-enable once type system is back
                //        checkArgumentTypes(matchedParams, functionType);

            }
        }
    }


    public ArgumentMatcher(RCallExpression callExpression) throws MatchingException {
        RFunctionExpression function = RPsiUtils.getFunction(callExpression);
        if (function != null) {
            functionType = new RFunctionType(function);
        } else {
            RType type = RTypeProvider.getType(callExpression.getExpression());
            if (!RFunctionType.class.isInstance(type)) {
                throw new MatchingException("function type does not match");
//                return Collections.emptyMap();
            }
            functionType = (RFunctionType) type;
        }
    }


    public void setFirstArgInjected(boolean firstArgInjected) {
        this.firstArgInjected = firstArgInjected;
    }


    public ArgumentsMatchResult checkArguments(RArgumentList argumentList) throws MatchingException {
        return matchArgs(argumentList.getExpressionList());
    }


    public ArgumentsMatchResult checkArguments(ROperatorExpression operatorExpression) throws MatchingException {
        List<RExpression> arguments = PsiTreeUtil.getChildrenOfTypeAsList(operatorExpression, RExpression.class);

        return matchArgs(arguments);
    }


    public ArgumentsMatchResult matchArgs(List<RExpression> arguments) throws MatchingException {
        List<RParameter> formalArguments = functionType.getFormalArguments();

        List<RExpression> suppliedArguments = new ArrayList<>(arguments);
        ArgumentsMatchResult matchResult = new ArgumentsMatchResult();

        exactMatching(formalArguments, suppliedArguments, matchResult);
        partialMatching(formalArguments, suppliedArguments, matchResult);
        positionalMatching(formalArguments, suppliedArguments, matchResult, functionType, firstArgInjected);

        return matchResult;
    }


    private static void partialMatching(List<RParameter> formalArguments,
                                        List<RExpression> suppliedArguments,
                                        ArgumentsMatchResult matchResult) throws MatchingException {
        matchParams(formalArguments, suppliedArguments, true, matchResult);
    }


    private static void exactMatching(List<RParameter> formalArguments,
                                      List<RExpression> suppliedArguments,
                                      ArgumentsMatchResult matchResult) throws MatchingException {
        matchParams(formalArguments, suppliedArguments, false, matchResult);
    }


    private static void matchParams(List<RParameter> parameters, List<RExpression> arguments,
                                    boolean usePartialMatching,
                                    ArgumentsMatchResult matchResult) throws MatchingException {
        List<RExpression> namedArguments = getNamedArguments(arguments);
        for (RExpression namedArg : namedArguments) {
            String name = namedArg.getName();
            List<RParameter> matches = getMatches(name, parameters, usePartialMatching);
            if (matches.size() > 1) {
                throw new MatchingException("formal argument " + name + " matched by multiply actual arguments");
            }
            if (matches.size() == 1) {
                matchResult.matchedParams.put(namedArg, matches.get(0));
            }
        }

        for (Map.Entry<RExpression, RParameter> entry : matchResult.matchedParams.entrySet()) {
            arguments.remove(entry.getKey());
            parameters.remove(entry.getValue());
        }
    }


    private static void positionalMatching(List<RParameter> formalArguments,
                                           List<RExpression> suppliedArguments,
                                           ArgumentsMatchResult matchResult,
                                           RFunctionType functionType,
                                           boolean isPipeInjected) throws MatchingException {

        List<RExpression> matchedArguments = new ArrayList<>();
        List<RParameter> matchedParameter = new ArrayList<>();
        int suppliedSize = suppliedArguments.size();
        boolean wasTripleDot = false;

        if (isPipeInjected) {
            formalArguments = formalArguments.subList(1, formalArguments.size());
        }

        for (int i = 0; i < formalArguments.size(); i++) {
            RParameter param = formalArguments.get(i);
            if (param.getText().equals("...")) {
                wasTripleDot = true;
                break;
            }
            if (i >= suppliedSize) {
                break;
            }
            RExpression arg = suppliedArguments.get(i);
            if (arg instanceof RAssignmentStatement && ((RAssignmentStatement) arg).isEqual()) {
                String argName = ((RAssignmentStatement) arg).getAssignee().getText();
                if (!argName.equals(param.getName())) {
                    wasTripleDot = true;
                    break;
                }
            }
            matchedArguments.add(arg);
            matchedParameter.add(param);
            matchResult.matchedParams.put(arg, param);
        }

        formalArguments.removeAll(matchedParameter);
        suppliedArguments.removeAll(matchedArguments);

        if (wasTripleDot) {
            matchResult.matchedByTripleDot.addAll(suppliedArguments);
            suppliedArguments.clear();
        }

        List<RParameter> unmatched = new ArrayList<RParameter>();
        for (RParameter parameter : formalArguments) {
            if (parameter.getText().equals("...")) {
                continue;
            }
            RExpression defaultValue = parameter.getExpression();
            if (defaultValue != null) {
                matchResult.matchedParams.put(defaultValue, parameter);
            } else {
                unmatched.add(parameter);
            }
        }

        if (!unmatched.isEmpty()) {
            unmatched.removeAll(functionType.getOptionalParams());
            if (!unmatched.isEmpty()) {
                throw new MatchingException(generateMissingArgErrorMessage(unmatched, 0));
            }
        }

        if (!suppliedArguments.isEmpty()) {
            checkUnmatchedArgs(suppliedArguments);
        }
    }


    private static void checkArgumentTypes(Map<RExpression, RParameter> matchedParams, RFunctionType functionType) throws MatchingException {
        for (Map.Entry<RExpression, RParameter> entry : matchedParams.entrySet()) {
            RParameter parameter = entry.getValue();
            RType paramType = RTypeProvider.getParamType(parameter, functionType);
            if (paramType == null || paramType instanceof RUnknownType) {
                continue;
            }

            boolean isOptional = functionType.isOptional(parameter.getName());
            RType argType = RTypeProvider.getType(entry.getKey());

            if (argType != null && !RUnknownType.class.isInstance(argType)) {
                if (!TypeUtil.matchTypes(paramType, argType, isOptional)) {
                    throw new MatchingException(parameter.getText() + " expected to be of type " + paramType +
                            ", found type " + argType);
                }
            }
        }
    }


    private static String generateMissingArgErrorMessage(List<RParameter> parameters, int i) {
        String noDefaultMessage = " missing, with no default";
        if (i == parameters.size() - 1) {
            return "argument \'" + parameters.get(i).getText() + "\' is" + noDefaultMessage;
        }
        StringBuilder stringBuilder = new StringBuilder("arguments ");
        while (i < parameters.size()) {
            stringBuilder.append("\"").append(parameters.get(i).getText()).append("\"").append(", ");
            i++;
        }
        int length = stringBuilder.length();
        return stringBuilder.delete(length - 2, length - 1).append("are").append(noDefaultMessage).toString();
    }


    private static List<RParameter> getMatches(String name, List<RParameter> parameters, boolean usePartial) {
        List<RParameter> matches = new ArrayList<RParameter>();
        for (RParameter param : parameters) {
            if (usePartial && param.getText().equals("...")) {
                return matches;
            }
            String paramName = param.getName();
            if (paramName != null) {
                if (usePartial) {
                    if (paramName.startsWith(name)) {
                        matches.add(param);
                    }
                } else {
                    if (paramName.equals(name)) {
                        matches.add(param);
                    }
                }
            }
        }
        return matches;
    }


    private static List<RExpression> getNamedArguments(List<RExpression> arguments) {
        List<RExpression> namedArgs = new ArrayList<RExpression>();
        for (RExpression arg : arguments) {
            if (arg instanceof RAssignmentStatement && arg.getName() != null) {
                namedArgs.add(arg);
            }
        }
        return namedArgs;
    }


    private static void checkUnmatchedArgs(List<RExpression> arguments) throws MatchingException {
        int size = arguments.size();
        if (size == 1) {
            throw new MatchingException("unused argument " + arguments.get(0).getText());
        }
        if (size > 0) {
            StringBuilder errorMessage = new StringBuilder("unused arguments: ");
            for (RExpression expression : arguments) {
                errorMessage.append(expression.getText()).append(", ");
            }
            int lastComma = errorMessage.lastIndexOf(",");
            throw new MatchingException(errorMessage.delete(lastComma, lastComma + 1).toString());
        }
    }


}
