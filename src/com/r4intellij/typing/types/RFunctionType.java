package com.r4intellij.typing.types;

import com.intellij.psi.PsiManager;
import com.r4intellij.RPsiUtils;
import com.r4intellij.RStaticAnalyzerHelper;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RParameter;
import com.r4intellij.typing.*;

import java.util.*;

public class RFunctionType extends RType {
    private final RFunctionExpression myFunctionExpression;
    private RType myReturnType;
    private List<RFunctionRule> myRules = new ArrayList<RFunctionRule>();
    private Map<String, RTypedParameter> myParameters = new HashMap<String, RTypedParameter>();


    public RFunctionType(RFunctionExpression functionExpression) {
        myFunctionExpression = functionExpression;
        List<RParameter> parameters = functionExpression.getParameterList().getParameterList();
        for (RParameter parameter : parameters) {
            String parameterName = parameter.getName();
            myParameters.put(parameterName, new RTypedParameter(parameterName, null, parameter));
        }
        createFunctionType();
    }


    public RFunctionType(RS4ClassType returnType) {
        myReturnType = returnType;
        myFunctionExpression = null;

        for (String slot : returnType.getSlots()) {
            RTypedParameter typedParameter = new RTypedParameter(slot, returnType.getSlotType(slot), null);
            typedParameter.setOptional(true);
            myParameters.put(slot, typedParameter);
        }
    }


    @Override
    public String getCanonicalName() {
        return "function";
    }


    @Override
    public RType resolveType(RTypeEnvironment env) {
        throw new UnsupportedOperationException();
    }


    private void createFunctionType() {
        RAssignmentStatement assignmentStatement = RPsiUtils.getAssignmentStatement(myFunctionExpression);
        if (assignmentStatement != null) {
            List<Substring> lines = DocStringUtil.getDocStringLines(assignmentStatement);
            for (Substring line : lines) {
                new RAnnotationParser(this).interpretLine(line);
            }
        }
        Set<String> strings = RStaticAnalyzerHelper.optionalParameters(myFunctionExpression);
        for (String name : strings) {
            RTypedParameter typedParameter = myParameters.get(name);
            if (typedParameter != null) {
                typedParameter.setOptional(true);
            }
        }
        for (RTypedParameter parameter : myParameters.values()) {
            Set<RType> parameterTypes = new HashSet<RType>();
            RType fromAnnotation = parameter.getType();
            if (fromAnnotation != null) {
                parameterTypes.add(fromAnnotation);
                RExpression defaultValue = parameter.getParameter().getExpression();
                if (defaultValue != null) {
                    RType defaultType = RTypeProvider.getType(defaultValue);
                    if (!RUnknownType.class.isInstance(defaultType)) {
                        parameterTypes.add(defaultType);
                    }
                }
            }
            if (!parameterTypes.isEmpty()) {
                parameter.setType(RUnionType.create(parameterTypes));
            }
        }
        if (myReturnType == null || myReturnType instanceof RUnknownType) {
            if (PsiManager.getInstance(myFunctionExpression.getProject()).isInProject(myFunctionExpression)) {
                RType type = RTypeProvider.guessReturnValueTypeFromBody(myFunctionExpression);
                if (!RUnknownType.class.isInstance(type)) {
                    myReturnType = type;
                }
            }
        }
    }


    public void setOptional(String paramName) {
        myParameters.get(paramName).setOptional(true);
    }


    public RType getReturnType() {
        return myReturnType;
    }


    public List<RFunctionRule> getRules() {
        return myRules;
    }


    public void addParameterType(String name, RType type) {
        myParameters.get(name).setType(type);
    }


    public RType getParameterType(String name) {
        return myParameters.get(name).getType();
    }


    public void setReturnType(RType returnType) {
        myReturnType = returnType;
    }


    public RFunctionExpression getFunctionExpression() {
        return myFunctionExpression;
    }


    public void addRule(RFunctionRule rule) {
        myRules.add(0, rule); // we are parsing from bottom to top
    }


    public List<RParameter> getOptionalParams() {
        List<RParameter> optionalParams = new ArrayList<RParameter>();
        for (RTypedParameter typedParameter : myParameters.values()) {
            if (typedParameter.isOptional()) {
                optionalParams.add(typedParameter.getParameter());
            }
        }
        return optionalParams;
    }


    @SuppressWarnings("SimplifiableIfStatement")
    public boolean isOptional(String param) {
        if (param != null && myParameters.containsKey(param)) {
            return myParameters.get(param).isOptional();
        }
        return true;
    }


    public List<RParameter> getFormalArguments() {
        return myFunctionExpression != null
                ? myFunctionExpression.getParameterList().getParameterList()
                : Collections.<RParameter>emptyList();
    }
}