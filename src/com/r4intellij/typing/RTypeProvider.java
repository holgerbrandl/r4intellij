package com.r4intellij.typing;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Processor;
import com.intellij.util.Query;
import com.r4intellij.RPsiUtils;
import com.r4intellij.RStaticAnalyzerHelper;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.types.*;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class RTypeProvider {


    public static final String LIST = "list";


    public static RType getType(RPsiElement element) {
        // disabled because cache implementation is buggy
//        return RTypeContext.getTypeFromCache(element);
        return RUnknownType.INSTANCE;
    }


    /**
     * evaluates type of expression
     */
    public static RType buildType(PsiElement element) {
        if (element == null) {
            return RUnknownType.INSTANCE;
        }
        if (element instanceof RStringLiteralExpression) {
            return RCharacterType.INSTANCE;
        }
        if (element instanceof RNumericLiteralExpression) {
            return getNumericType((RNumericLiteralExpression) element);
        }
        if (element instanceof RLogicalLiteralExpression) {
            return RLogicalType.INSTANCE;
        }
        if (element instanceof RNullLiteralExpression) {
            return RNullType.INSTANCE;
        }
        if (element instanceof RNaLiteralExpression) {
            return ((RNaLiteralExpression) element).getType();
        }

        if (element instanceof RReferenceExpression) {
            return getReferenceExpressionType((RReferenceExpression) element);
        }

        if (element instanceof RAssignmentStatement) {
            RPsiElement assignedValue = ((RAssignmentStatement) element).getAssignedValue();
            if (assignedValue != null) {
                return getType(assignedValue);
            }
        }

        if (element instanceof RCallExpression) {
            return getCallExpressionType((RCallExpression) element);
        }

        if (element instanceof RFunctionExpression) {
            return new RFunctionType((RFunctionExpression) element);
        }
        if (element instanceof RSubscriptionExpression) {
            RSubscriptionExpression subscriptionExpression = (RSubscriptionExpression) element;
            boolean isSingleBracket = subscriptionExpression.getLbracket() != null;
            List<RExpression> expressionList = subscriptionExpression.getExpressionList();
            RExpression base = expressionList.get(0);
            if (base != null) {
                RType type = getType(base);
                return type.getSubscriptionType(expressionList.subList(1, expressionList.size()), isSingleBracket);
            }
        }
        if (element instanceof RBlockExpression) {
            RBlockExpression blockExpression = (RBlockExpression) element;
            List<RExpression> expressionList = blockExpression.getExpressionList();
            if (!expressionList.isEmpty()) {
                RExpression lastExpression = expressionList.get(expressionList.size() - 1);
                return RTypeProvider.getType(lastExpression);
            }
        }

        if (element instanceof RIfStatement) {
            RIfStatement ifStatement = (RIfStatement) element;
            List<RExpression> expressions = ifStatement.getExpressionList();
            if (expressions.size() > 1) {
                Set<RType> types = new HashSet<RType>();
                for (int i = 1; i < expressions.size(); i++) {
                    RType type = getType(expressions.get(i));
                    if (!RUnknownType.class.isInstance(type)) {
                        types.add(type);
                    }
                }
                return RUnionType.create(types);
            }
        }

        if (element instanceof RMemberExpression) {
            RMemberExpression memberExpression = (RMemberExpression) element;
            RType elementType = getType(memberExpression.getExpression());
            return elementType.getMemberType(memberExpression.getTag());
        }
        if (element instanceof RSliceExpression) {
            return RNumericType.INSTANCE; // TODO: think!
        }
        if (element instanceof ROperatorExpression) {
            return getBinaryExpressionType((ROperatorExpression) element);
        }
        if (element instanceof RAtExpression) {
            RAtExpression atExpression = (RAtExpression) element;
            RExpression base = atExpression.getExpression();
            String tag = atExpression.getTag();
            RType baseType = getType(base);
            return baseType.getSlotType(tag);
        }
        return RUnknownType.INSTANCE;
    }


    private static RType getNumericType(RNumericLiteralExpression expression) {
        if (expression.getInteger() != null) {
            return RIntegerType.INSTANCE;
        }
        if (expression.getComplex() != null) {
            return RComplexType.INSTANCE;
        }
        return expression.getNumeric() != null ? RNumericType.INSTANCE : RUnknownType.INSTANCE;
    }


    private static RType getBinaryExpressionType(ROperatorExpression expression) {
        RFunctionExpression function = RPsiUtils.getFunction(expression);
        if (function == null) {
            return RUnknownType.INSTANCE;
        }
        RType functionType = getType(function);
        if (!RFunctionType.class.isInstance(functionType)) {
            return RUnknownType.INSTANCE; // TODO: Error?
        }
        List<RExpression> arguments = PsiTreeUtil.getChildrenOfTypeAsList(expression, RExpression.class);
        return getFunctionCallReturnType((RFunctionType) functionType, arguments);
    }


    private static RType getCallExpressionType(RCallExpression element) {
        String functionName = element.getExpression().getName();
        if (LIST.equals(functionName)) {
            return getNewListType(element.getArgumentList().getExpressionList());
        }
        if ("setClass".equals(functionName)) {
            return getClassConstructor(element);
        }
        if ("new".equals(functionName)) {
            return getTypeFromNew(element);
        }

        RFunctionExpression function = RPsiUtils.getFunction(element);
        final RFunctionType functionType;
        if (function != null) {
            functionType = new RFunctionType(function);
        } else {
            RType type = RTypeProvider.getType(element.getExpression());
            if (!RFunctionType.class.isInstance(type)) {
                return RUnknownType.INSTANCE;
            }
            functionType = (RFunctionType) type;
        }

        return getFunctionCallReturnType(functionType, element.getArgumentList().getExpressionList());
    }


    private static RType getClassConstructor(RCallExpression element) {
        RS4ClassType s4Class = RS4ClassType.createFromSetClass(element);
        if (s4Class != null) {
            // TODO : check S4 params
            return new RFunctionType(s4Class);
        }
        return RUnknownType.INSTANCE;
    }


    private static RType getTypeFromNew(RCallExpression callExpression) {
        String name = null;
        RExpression classExpression = RPsiUtils.findParameterValue("Class", callExpression);
        if (classExpression instanceof RStringLiteralExpression) {
            name = classExpression.getText().substring(1, classExpression.getText().length() - 1);
        }
        if (name == null) {
            return RUnknownType.INSTANCE;
        }
        RType atomicType = findTypeByName(name);
        if (atomicType != null) {
            return atomicType;
        }
        if ("list".equals(name)) {
            List<RExpression> arguments = callExpression.getArgumentList().getExpressionList();
            arguments.remove(classExpression);
            return getNewListType(arguments);
        }

        RS4ClassType s4Class = RS4ClassType.byName(callExpression.getProject(), name);
        if (s4Class == null) {
            return RUnknownType.INSTANCE;
        }
        for (RExpression argument : callExpression.getArgumentList().getExpressionList()) {
            if (argument instanceof RAssignmentStatement) {
                PsiElement assignee = ((RAssignmentStatement) argument).getAssignee();
                RPsiElement value = ((RAssignmentStatement) argument).getAssignedValue();
                if (assignee instanceof RReferenceExpression) {
                    String slotName = assignee.getText();
                    if (!s4Class.hasSlot(slotName)) {
                        return new RErrorType("Invalid slot " + slotName);
                    }
                    if (!RTypeChecker.matchTypes(s4Class.getSlotType(slotName), getType(value))) {
                        return new RErrorType("Wrong type of slot " + slotName);
                    }
                }
            }
        }
        return s4Class;
    }


    private static RType getFunctionCallReturnType(RFunctionType functionType, List<RExpression> arguments) {
        // step 1: check @return
        if (functionType.getReturnType() != null) {
            return functionType.getReturnType();
        }

        Map<RExpression, RParameter> matchedParams = new HashMap<RExpression, RParameter>();
        List<RExpression> matchedByTripleDot = new ArrayList<RExpression>();

        try {
            RTypeChecker.matchArgs(arguments, matchedParams, matchedByTripleDot, functionType);
        } catch (MatchingException e) {
            return RUnknownType.INSTANCE;
        }

        Map<String, RParameterConfiguration> paramToSuppliedConfiguration =
                new HashMap<String, RParameterConfiguration>();

        // step 2: check @type annotation
        if (!isMatchedTypes(functionType, matchedParams, matchedByTripleDot, paramToSuppliedConfiguration)) {
            return RUnknownType.INSTANCE;
        }

        //step 3: check @rule
        return tryApplyRule(functionType, paramToSuppliedConfiguration);
    }


    private static RType getNewListType(List<RExpression> arguments) {
        RListType list = new RListType();
        for (RExpression arg : arguments) {
            if (arg instanceof RAssignmentStatement) {
                RAssignmentStatement assignment = (RAssignmentStatement) arg;
                list.addField(assignment.getAssignee().getText(), getType(assignment.getAssignedValue()));
            } else {
                list.addField(getType(arg));
            }
        }
        return list;
    }


    private static boolean isMatchedTypes(RFunctionType functionType,
                                          Map<RExpression, RParameter> matchedParams,
                                          List<RExpression> matchedByTripleDot,
                                          Map<String, RParameterConfiguration> paramToSuppliedConfiguration) {
        for (Map.Entry<RExpression, RParameter> entry : matchedParams.entrySet()) {
            RExpression expr = entry.getKey();
            RParameter parameter = entry.getValue();

            if (expr instanceof RAssignmentStatement) {
                expr = (RExpression) ((RAssignmentStatement) expr).getAssignedValue();
            }

            RType exprType = getType(expr);
            String paramName = parameter.getName();
            paramToSuppliedConfiguration.put(paramName, new RParameterConfiguration(exprType, expr));
            RType paramType = functionType.getParameterType(paramName);

            if (paramType != null && !paramType.equals(exprType)) {
                // can't match
                return false;
            }
        }

        if (!matchedByTripleDot.isEmpty()) {
            List<RType> types = new ArrayList<RType>();
            for (RExpression expr : matchedByTripleDot) {
                types.add(getType(expr));
            }
            paramToSuppliedConfiguration.put("...", new RParameterConfiguration(new RTypeSequence(types), null));
        }

        return true;
    }


    public static RType getParamType(RParameter parameter, RFunctionType functionType) {
        RType type = functionType.getParameterType(parameter.getName());
        if (type != null) {
            return type;
        }
        return RUnknownType.INSTANCE;
    }


    private static RType tryApplyRule(RFunctionType functionType,
                                      Map<String, RParameterConfiguration> paramToSuppliedConfiguration) {
        rulefor:
        for (RFunctionRule rule : functionType.getRules()) {
            RTypeEnvironment env = new RTypeEnvironment();
            for (Map.Entry<String, RParameterConfiguration> entry : rule.getParameters().entrySet()) {
                String paramName = entry.getKey();
                RParameterConfiguration conf = entry.getValue();
                RParameterConfiguration exprConf = paramToSuppliedConfiguration.get(paramName);
                if (exprConf == null) {
                    continue rulefor;
                }
                RType ruleType = conf.getType();
                if (ruleType != null) {
                    if (ruleType instanceof RTypeVariable) {
                        RTypeVariable typeVariable = (RTypeVariable) ruleType;
                        String variableName = typeVariable.getName();
                        if (!env.contains(variableName)) {
                            env.addType(variableName, exprConf.getType());
                        }
                        ruleType = env.getType(variableName);
                    }
                    if (!RTypeChecker.matchTypes(ruleType, exprConf.getType())) {
                        continue rulefor;
                    }
                }
                RExpression ruleValue = conf.getValue();
                if (ruleValue != null) {
                    RExpression exprValue = exprConf.getValue();

                    // TODO : evaluate expressions?
                    if (exprValue == null || !matchExpressions(exprValue, ruleValue)) {
                        continue rulefor;
                    }
                }
            }
            return rule.getReturnType().resolveType(env);
        }
        return RUnknownType.INSTANCE;
    }


    private static boolean matchExpressions(RExpression substitution, RExpression base) {
        if (base instanceof RReferenceExpression) {
            String name = base.getName();
            if ("string".equals(name) && substitution instanceof RStringLiteralExpression) {
                return true;
            }
            if ("number".equals(name) && substitution instanceof RNumericLiteralExpression) {
                return true;
            }
        }
        return substitution.getText().equals(base.getText());
    }


    @Nullable
    private static RType guessTypeFromFunctionBody(final RParameter parameter) {
        final RType[] type = new RType[1];
        type[0] = null;
        RFunctionExpression function = RPsiUtils.getFunction(parameter);
        if (function == null) {
            return null;
        }
        final RBlockExpression blockExpression = PsiTreeUtil.getChildOfType(function, RBlockExpression.class);
        if (blockExpression == null) {
            return null;
        }
        Query<PsiReference> references = ReferencesSearch.search(parameter);
        references.forEach(new Processor<PsiReference>() {
            @Override
            public boolean process(PsiReference reference) {
                PsiElement element = reference.getElement();
                PsiElement parent = element.getParent();
                //TODO: check operations more strict
                //TODO: check control flow analysis
                if (parent instanceof ROperatorExpression) {
                    if (PsiTreeUtil.isAncestor(blockExpression, element, false)) {
                        type[0] = RNumericType.INSTANCE;
                        return false;
                    }
                }
                return true;
            }
        });
        return type[0];
    }


    @Nullable
    public static RType findTypeByName(String typeName) {
        if (typeName.equals("numeric")) {
            return RNumericType.INSTANCE;
        }
        if (typeName.equals("character")) {
            return RCharacterType.INSTANCE;
        }
        if (typeName.equals("logical")) {
            return RLogicalType.INSTANCE;
        }
        if (typeName.equals("complex")) {
            return RComplexType.INSTANCE;
        }
        if (typeName.equals("integer")) {
            return RIntegerType.INSTANCE;
        }
        if (typeName.equals("null")) {
            return RNullType.INSTANCE;
        }
        if (typeName.equals("raw")) {
            return RRawType.INSTANCE;
        }
        //we need to return null in this case to create type variable
        return null;
    }


    private static RType getReferenceExpressionType(RReferenceExpression expression) {
        PsiReference reference = expression.getReference();
        if (reference == null) {
            return RUnknownType.INSTANCE;
        }
        return RStaticAnalyzerHelper.getReferenceType(expression);
    }


    public static RType guessReturnValueTypeFromBody(RFunctionExpression functionExpression) {
        RExpression expression = functionExpression.getExpression();
        if (expression == null) {
            return RUnknownType.INSTANCE;
        }
        Set<RType> types = new HashSet<RType>();
        RType type = getType(expression);
        if (!RUnknownType.class.isInstance(type)) {
            types.add(type);
        }
        collectReturnTypes(functionExpression, types);
        return RUnionType.create(types);
    }


    private static void collectReturnTypes(RFunctionExpression functionExpression, Set<RType> types) {
        RCallExpression[] calls = RPsiUtils.getAllChildrenOfType(functionExpression, RCallExpression.class);
        for (RCallExpression callExpression : calls) {
            if (RPsiUtils.isReturn(callExpression)) {
                List<RExpression> args = callExpression.getArgumentList().getExpressionList();
                if (args.size() == 1) {
                    RType rType = RTypeProvider.getType(args.iterator().next());
                    if (rType != null && !RUnknownType.class.isInstance(rType)) {
                        types.add(rType);
                    }
                }
            }
        }
    }


    public static boolean isSubtype(RType subType, RType type) {
        return type.getClass().isAssignableFrom(subType.getClass());
    }
}
