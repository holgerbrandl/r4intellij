package com.r4intellij;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.RTypeProvider;
import com.r4intellij.typing.types.RType;
import com.r4intellij.typing.types.RUnionType;
import com.r4intellij.typing.types.RUnknownType;

import java.util.*;

public class RStaticAnalyzerHelper {

    private interface StaticAnalysisResult {
        StaticAnalysisResult applyRead(RReferenceExpression ref);


        StaticAnalysisResult applyAssign(RAssignmentStatement assignmentStatement);


        StaticAnalysisResult applyAssignInFor(RExpression assignee, RExpression expression);


        StaticAnalysisResult merge(StaticAnalysisResult other);


        void applyReturn();


        boolean isEnd();
    }


    private static class OptionalParameters implements StaticAnalysisResult {

        private Set<String> myPossibleOptionals = new HashSet<String>();
        private Set<String> myOptionals = new HashSet<String>();
        private Set<String> myRead = new HashSet<String>();


        private OptionalParameters(List<RParameter> parameters) {
            for (RParameter parameter : parameters) {
                if (parameter.getName() != null && parameter.getExpression() == null) {
                    myPossibleOptionals.add(parameter.getName());
                }
            }
        }


        public OptionalParameters(OptionalParameters parameters) {
            myPossibleOptionals = parameters.myPossibleOptionals;
            myOptionals.addAll(parameters.myOptionals);
            myRead.addAll(parameters.myRead);
        }


        @Override
        public StaticAnalysisResult applyRead(RReferenceExpression ref) {
            PsiElement parent = ref.getParent();
            if (parent instanceof RArgumentList) {
                parent = parent.getParent();
                if (parent instanceof RCallExpression) {
                    RCallExpression callExpression = (RCallExpression) parent;
                    String functionName = callExpression.getExpression().getText();
                    if ("missing".equals(functionName) || "is.null".equals(functionName)) {
                        return this;
                    }
                }
            }
            String name = ref.getName();
            if (myPossibleOptionals.contains(name) && !myOptionals.contains(name)) {
                OptionalParameters newParams = new OptionalParameters(this);
                newParams.myRead.add(name);
                return newParams;
            }
            return this;
        }


        private StaticAnalysisResult applyWrite(String name) {
            if (myPossibleOptionals.contains(name) && !myRead.contains(name)) {
                OptionalParameters newParams = new OptionalParameters(this);
                newParams.myOptionals.add(name);
                return newParams;
            }
            return this;
        }


        @Override
        public StaticAnalysisResult applyAssign(RAssignmentStatement assignmentStatement) {
            PsiElement assignee = assignmentStatement.getAssignee();
            if (assignee == null) {
                return this;
            }
            return applyWrite(assignee.getText());
        }


        @Override
        public StaticAnalysisResult applyAssignInFor(RExpression assignee, RExpression expression) {
            return applyWrite(assignee.getText());
        }


        @Override
        public StaticAnalysisResult merge(StaticAnalysisResult other) {
            if (!(other instanceof OptionalParameters)) {
                throw new IllegalArgumentException();
            }
            OptionalParameters otherParams = (OptionalParameters) other;
            myOptionals.addAll(otherParams.myOptionals);
            Set<String> mergeRead = new HashSet<String>();
            for (String name : myRead) {
                if (otherParams.myRead.contains(name)) {
                    mergeRead.add(name);
                }
            }
            myRead = mergeRead;
            return this;
        }


        @Override
        public void applyReturn() {
            for (String name : myPossibleOptionals) {
                if (!myRead.contains(name)) {
                    myOptionals.add(name);
                }
            }
        }


        @Override
        public boolean isEnd() {
            return false;
        }


        public Set<String> getOptionalParameters() {
            return myOptionals;
        }
    }


    private static class ReachTypes implements StaticAnalysisResult {
        final Map<String, RType> myReachTypes = new HashMap<String, RType>();
        final RReferenceExpression myWhat;
        RType myResult = null;


        public ReachTypes(RReferenceExpression what) {
            myWhat = what;
        }


        private ReachTypes(ReachTypes other) {
            myReachTypes.putAll(other.myReachTypes);
            myWhat = other.myWhat;
            myResult = other.myResult;
        }


        public RType getResultType() {
            if (myResult != null) {
                return myResult;
            }
            return RUnknownType.INSTANCE;
        }


        @Override
        public StaticAnalysisResult applyRead(RReferenceExpression ref) {
            String name = ref.getName();
            RType refType = RUnknownType.INSTANCE;
            if (myReachTypes.containsKey(name)) {
                refType = myReachTypes.get(name);
            }
            //RTypeContext.putTypeInCache(ref, refType);
            if (myResult == null && ref.equals(myWhat)) {
                myResult = refType;
            }
            return this;
        }


        @Override
        public StaticAnalysisResult applyAssign(RAssignmentStatement assignmentStatement) {
            PsiElement assignee = assignmentStatement.getAssignee();
            RPsiElement assignedValue = assignmentStatement.getAssignedValue();
            if (assignedValue == null) {
                return this;
            }
            RType assignedValueType = RTypeProvider.getType(assignedValue);
            ReachTypes result = this;
            if (assignee instanceof RReferenceExpression) {
                RReferenceExpression ref = (RReferenceExpression) assignee;
                String name = ref.getName();
                result = new ReachTypes(this);
                result.myReachTypes.put(name, assignedValueType);
            }
            if (assignee instanceof RMemberExpression) {
                RMemberExpression member = (RMemberExpression) assignee;
                RExpression base = member.getExpression();
                String tag = member.getTag();
                if (base instanceof RReferenceExpression) { // check only x$y not f()$y
                    RReferenceExpression ref = (RReferenceExpression) base;
                    String name = ref.getName();
                    RType baseType = myReachTypes.get(name);
                    if (baseType == null) {
                        baseType = RUnknownType.INSTANCE;
                    }
                    final Set<RType> beforeTypes;
                    if (baseType instanceof RUnionType) {
                        beforeTypes = ((RUnionType) baseType).getTypes();
                    } else {
                        beforeTypes = new HashSet<RType>();
                        beforeTypes.add(baseType);
                    }
                    Set<RType> afterTypes = new HashSet<RType>();
                    for (RType type : beforeTypes) {
                        afterTypes.add(type.afterMemberType(tag, assignedValueType));
                    }
                    RType resultType = RUnionType.create(afterTypes);
                    result = new ReachTypes(this);
                    result.myReachTypes.put(name, resultType);
                }
            }
            if (assignee instanceof RSubscriptionExpression) {
                RSubscriptionExpression subscriptionExpression = (RSubscriptionExpression) assignee;
                List<RExpression> expressionList = subscriptionExpression.getExpressionList();
                RExpression base = expressionList.get(0);
                List<RExpression> arguments = expressionList.subList(1, expressionList.size());
                if (base instanceof RReferenceExpression) {
                    String name = base.getName();
                    RType baseType = myReachTypes.get(name);
                    if (baseType == null) {
                        baseType = RUnknownType.INSTANCE;
                    }
                    boolean isSingleBracket = subscriptionExpression.getLbracket() != null;
                    RType resultType = baseType.afterSubscriptionType(arguments, assignedValueType, isSingleBracket);
                    result = new ReachTypes(this);
                    result.myReachTypes.put(name, resultType);
                }
            }

            if (assignee instanceof RCallExpression) {
                RCallExpression callExpression = (RCallExpression) assignee;
                RExpression function = callExpression.getExpression();
                if (function instanceof RReferenceExpression) {
                    String functionName = function.getName();
                    List<RExpression> arguments = callExpression.getArgumentList().getExpressionList();
                    if ("class".equals(functionName) && arguments.size() == 1) { // add S3Class
                        RExpression arg = arguments.get(0);
                        if (arg instanceof RReferenceExpression) {
                            String name = arg.getName();
                            List<String> s3Classes = new ArrayList<String>();
                            if (assignedValue instanceof RStringLiteralExpression) {
                                String quoted = assignedValue.getText();
                                String s3Class = quoted.substring(1, quoted.length() - 1);
                                s3Classes.add(s3Class);
                            }
                            if (assignedValue instanceof RCallExpression) {
                                RCallExpression assignedValueCall = (RCallExpression) assignedValue;
                                if ("c".equals(assignedValueCall.getExpression().getName())) {
                                    for (RExpression s3ClassExpr : assignedValueCall.getArgumentList().getExpressionList()) {
                                        if (s3ClassExpr instanceof RStringLiteralExpression) {
                                            String quoted = s3ClassExpr.getText();
                                            String s3Class = quoted.substring(1, quoted.length() - 1);
                                            s3Classes.add(s3Class);
                                        }
                                    }
                                }
                            }
                            if (!s3Classes.isEmpty() && myReachTypes.containsKey(name)) {
                                result = new ReachTypes(this);
                                result.myReachTypes.put(name, myReachTypes.get(name).replaceS3Types(s3Classes));
                            }
                        }
                    }
                }
            }

            return result;
        }


        @Override
        public StaticAnalysisResult applyAssignInFor(RExpression assignee, RExpression expression) {
            if (!RReferenceExpression.class.isInstance(assignee)) {
                return this;
            }
            String name = assignee.getName();
            ReachTypes result = new ReachTypes(this);
            result.myReachTypes.put(name, RTypeProvider.getType(expression).getElementTypes());
            return result;
        }


        @Override
        public StaticAnalysisResult merge(StaticAnalysisResult other) {
            if (!ReachTypes.class.isInstance(other)) {
                throw new IllegalArgumentException();
            }
            ReachTypes result = new ReachTypes((ReachTypes) other);
            for (Map.Entry<String, RType> entry : myReachTypes.entrySet()) {
                String name = entry.getKey();
                RType type = entry.getValue();
                if (result.myReachTypes.containsKey(name)) {
                    Set<RType> typeSet = new HashSet<RType>();
                    typeSet.add(type);
                    typeSet.add(result.myReachTypes.get(name));
                    type = RUnionType.create(typeSet);
                }
                result.myReachTypes.put(name, type);
            }
            if (result.myResult == null && myResult != null) {
                result.myResult = myResult;
            }
            return result;
        }


        @Override
        public void applyReturn() {

        }


        @Override
        public boolean isEnd() {
            return false;
        }
    }


    public static RType getReferenceType(RReferenceExpression what) {
        ReachTypes resolveResult = new ReachTypes(what);
        RFunctionExpression functionExpression = PsiTreeUtil.getParentOfType(what, RFunctionExpression.class);
        if (functionExpression != null) {
            resolveResult = (ReachTypes) analyze(functionExpression.getExpression(), resolveResult);
        } else {
            PsiFile file = what.getContainingFile();
            RExpression[] expressions = PsiTreeUtil.getChildrenOfType(file, RExpression.class);
            assert expressions != null;
            resolveResult = (ReachTypes) analyzeSequence(Arrays.asList(expressions), resolveResult);
        }
        return resolveResult.getResultType();
    }


    public static Set<String> optionalParameters(RFunctionExpression function) {
        RExpression functionExpression = function.getExpression();
        if (!RBlockExpression.class.isInstance(functionExpression)) {
            return new HashSet<String>();
        }
        OptionalParameters parameters = new OptionalParameters(function.getParameterList().getParameterList());
        parameters = (OptionalParameters) analyze(functionExpression, parameters);
        parameters.applyReturn();
        return parameters.getOptionalParameters();
    }


    private static StaticAnalysisResult analyze(RPsiElement where, StaticAnalysisResult parentResult) {
        //TODO: find better way --- can we use just ==
        if (where instanceof RReferenceExpression) {
            return parentResult.applyRead((RReferenceExpression) where);
        }

        if (where instanceof RAssignmentStatement) {
            RAssignmentStatement assignment = (RAssignmentStatement) where;
            StaticAnalysisResult resolveResult = analyze(assignment.getAssignedValue(), parentResult);
            if (resolveResult.isEnd()) {
                return resolveResult;
            }
            resolveResult = resolveResult.applyAssign(assignment);
            return resolveResult;
        }

        if (where instanceof RIfStatement) {
            RIfStatement ifStatement = (RIfStatement) where;
            List<RExpression> list = ifStatement.getExpressionList();
            RExpression condition = list.get(0);
            StaticAnalysisResult conditionResult = analyze(condition, parentResult);
            if (conditionResult.isEnd()) {
                return conditionResult;
            }
            StaticAnalysisResult thenResult = analyze(list.get(1), conditionResult);
            if (thenResult.isEnd()) {
                return thenResult;
            }
            StaticAnalysisResult elseResult = conditionResult;
            if (list.size() == 3) {
                elseResult = analyze(list.get(2), conditionResult);
                if (elseResult.isEnd()) {
                    return elseResult;
                }
            }
            return thenResult.merge(elseResult);
        }

        if (where instanceof ROperatorExpression || where instanceof RPrefixExpression) {
            RExpression[] expressions = PsiTreeUtil.getChildrenOfType(where, RExpression.class);
            if (expressions == null) {
                return parentResult;
            }
            return analyzeSequence(Arrays.asList(expressions), parentResult);
        }

        if (where instanceof RBlockExpression) {
            RBlockExpression block = (RBlockExpression) where;
            return analyzeSequence(block.getExpressionList(), parentResult);
        }

        if (where instanceof RParenthesizedExpression) {
            RParenthesizedExpression parenthesized = (RParenthesizedExpression) where;
            return analyze(parenthesized.getExpression(), parentResult);
        }

        if (where instanceof RWhileStatement) {
            RWhileStatement whileStatement = (RWhileStatement) where;
            List<RExpression> list = whileStatement.getExpressionList();
            StaticAnalysisResult conditionResult = analyze(list.get(0), parentResult);
            if (conditionResult.isEnd()) {
                return conditionResult;
            }
            StaticAnalysisResult bodyResult = analyze(list.get(1), conditionResult);
            if (bodyResult.isEnd()) {
                return bodyResult;
            }
            return bodyResult.merge(conditionResult);
        }

        if (where instanceof RRepeatStatement) {
            RRepeatStatement repeatStatement = (RRepeatStatement) where;
            StaticAnalysisResult resolveResult = analyze(repeatStatement.getExpression(), parentResult);
            if (resolveResult.isEnd()) {
                return resolveResult;
            }
            return resolveResult.merge(parentResult);
        }

        if (where instanceof RCallExpression) {
            RCallExpression callExpression = (RCallExpression) where;
            List<RExpression> expressions = new ArrayList<RExpression>();
            expressions.add(callExpression.getExpression());
            for (RExpression argument : callExpression.getArgumentList().getExpressionList()) {
                RExpression child = argument;
                if (argument instanceof RAssignmentStatement) {
                    child = (RExpression) ((RAssignmentStatement) argument).getAssignedValue();
                }
                expressions.add(child);
            }
            StaticAnalysisResult result = analyzeSequence(expressions, parentResult);
            if (result.isEnd()) {
                return result;
            }
            if ("return".equals(callExpression.getExpression().getText())) {
                result.applyReturn();
            }
            return result;
        }

        if (where instanceof RSubscriptionExpression) {
            RSubscriptionExpression subscriptionExpression = (RSubscriptionExpression) where;
            return analyzeSequence(subscriptionExpression.getExpressionList(), parentResult);
        }

        if (where instanceof RSliceExpression) {
            RSliceExpression sliceExpression = (RSliceExpression) where;
            return analyzeSequence(sliceExpression.getExpressionList(), parentResult);
        }

        if (where instanceof RMemberExpression) {
            RMemberExpression memberExpression = (RMemberExpression) where;
            return analyze(memberExpression.getExpression(), parentResult);
        }
        if (where instanceof RForStatement) {
            RForStatement forStatement = (RForStatement) where;
            StaticAnalysisResult afterRange = analyze(forStatement.getRange(), parentResult);
            if (afterRange.isEnd()) {
                return afterRange;
            }
            StaticAnalysisResult withoutBody = afterRange.applyAssignInFor(forStatement.getTarget(), forStatement.getRange());
            if (withoutBody.isEnd()) {
                return withoutBody;
            }
            StaticAnalysisResult withBody = analyze(forStatement.getBody(), withoutBody);
            if (withBody.isEnd()) {
                return withBody;
            }
            return withoutBody.merge(withBody);
        }
        if (where instanceof RAtExpression) {
            RAtExpression atExpression = (RAtExpression) where;
            return analyze(atExpression.getExpression(), parentResult);
        }
        //TODO: look at other expressions (for, call expression)
        return parentResult;
    }


    private static StaticAnalysisResult analyzeSequence(List<RExpression> expressions, StaticAnalysisResult parentResult) {
        StaticAnalysisResult resolveResult = parentResult;
        for (RExpression expression : expressions) {
            resolveResult = analyze(expression, resolveResult);
            if (resolveResult.isEnd()) {
                return resolveResult;
            }
        }
        return resolveResult;
    }
}
