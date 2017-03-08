package com.r4intellij.inspections;

import com.google.common.base.Joiner;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RPsiImplUtil;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RParameter;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Holger Brandl
 */
@SuppressWarnings("SimplifiableIfStatement")
public class UnquotedArgsRule implements Serializable {

    private static final long serialVersionUID = 4858882213447068456L;

    private String packageName;
    private String methodName;
    private Collection<String> unquotedArgs;


    // dplyr::count[... wt sort
    public static UnquotedArgsRule fromString(String stringifiedRule) {
        Pattern logEntry = Pattern.compile("([A-z]*)::([A-z.][A-z0-9._]*)\\[(.*?)\\]");
        Matcher matchPattern = logEntry.matcher(stringifiedRule);

        UnquotedArgsRule rule = new UnquotedArgsRule();
        if (matchPattern.find()) {
            rule.unquotedArgs = Arrays.asList(matchPattern.group(3).split(" "));
            rule.packageName = matchPattern.group(1);
            rule.methodName = matchPattern.group(2);
        } else {
            System.err.println("Could not parse pattern: " + stringifiedRule);
            return null;
        }

        return rule;
    }


    @Override
    public String toString() {
        return packageName + "::" + methodName + "[" + Joiner.on(" ").join(unquotedArgs) + "]";
    }


    public boolean matches(RParameter rParameter) {
        RFunctionExpression functionExpression = PsiTreeUtil.getParentOfType(rParameter, RFunctionExpression.class);

        if (functionExpression == null) return false;

        String methodName = RPsiImplUtil.getName(functionExpression);
        if (methodName == null) return false;


        if (!Objects.equals(this.methodName, methodName)) return false;

        return unquotedArgs.contains("*") || unquotedArgs.contains(rParameter.getName());
    }


    public boolean matchesTripleDot(RFunctionExpression functionExpression) {
        if (functionExpression == null) return false;

        String methodName = RPsiImplUtil.getName(functionExpression);
        if (methodName == null) return false;

        if (!Objects.equals(this.methodName, methodName)) return false;

        return unquotedArgs.contains("*") || unquotedArgs.contains("...");
    }
}
