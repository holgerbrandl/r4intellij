package com.r4intellij.typing;

import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RParameter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Holger Brandl
 */
public class ArgumentsMatchResult {
    public final Map<RExpression, RParameter> matchedParams = new HashMap<>();
    public final List<RExpression> matchedByTripleDot = new ArrayList<>();
}
