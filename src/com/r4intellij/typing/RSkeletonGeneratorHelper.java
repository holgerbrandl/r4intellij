package com.r4intellij.typing;

import com.r4intellij.documentation.RHelpParser;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RParameter;
import com.r4intellij.typing.types.*;

import java.util.*;

public class RSkeletonGeneratorHelper {
    public static final Map<String, RType> TYPES = new HashMap<String, RType>();

    static {
        TYPES.put("logical", RLogicalType.INSTANCE);
        TYPES.put("complex", RComplexType.INSTANCE);
        TYPES.put("numeric", RNumericType.INSTANCE);
        TYPES.put("integer", RIntegerType.INSTANCE);
        TYPES.put("number", RComplexType.INSTANCE);
        TYPES.put("raw", RRawType.INSTANCE);
        TYPES.put("string", RCharacterType.INSTANCE);
        TYPES.put("character", RCharacterType.INSTANCE);
        TYPES.put("name", RCharacterType.INSTANCE);
    }

    public static void parseArgumentsDescription(String description, List<RParameter> parameters, Map<RParameter, RType> parsedTypes) {
        Map<RParameter, String> argsDesc = new HashMap<RParameter, String>();
        String[] argTexts = description.split("\n\n");
        for (String argText : argTexts) {
            String[] split = argText.split(":", 2);
            if (split.length < 2) {
                continue;
            }
            String arguments = split[0];
            String[] argNames = arguments.split(",");
            for (String argName : argNames) {
                String name = argName.trim();
                RParameter parameter = findParameter(name, parameters);
                if (parameter != null) {
                    argsDesc.put(parameter, split[1]);
                }
            }
        }
        for (Map.Entry<RParameter, String> entry : argsDesc.entrySet()) {
            RParameter parameter = entry.getKey();
            String text = entry.getValue();
            RType type = findType(text);
            if (!RUnknownType.class.isInstance(type)) {
                parsedTypes.put(parameter, type);
            }
        }
    }


    public static RType findType(String text) {
        Set<RType> foundTypes = new HashSet<RType>();
        String[] words = text.split("[^a-zA-Z/-]");
        for (String word : words) {
            if (word.isEmpty()) {
                continue;
            }
            RType type = RSkeletonGeneratorHelper.TYPES.get(word);
            if (type != null) {
                foundTypes.add(type);
            }
        }
        return RUnionType.create(foundTypes);
    }


    public static Map<RParameter, RType> guessArgsTypeFromHelp(RHelpParser help, RFunctionExpression function) {
        List<RParameter> parameters = function.getParameterList().getParameterList();
        Map<RParameter, RType> parsedTypes = new HashMap<RParameter, RType>();
        String argumentsDescription = help.getDocArguments();
        if (argumentsDescription != null && !parameters.isEmpty()) {
            RSkeletonGeneratorHelper.parseArgumentsDescription(argumentsDescription, parameters, parsedTypes);
        }
        return parsedTypes;
    }


    private static RParameter findParameter(String name, List<RParameter> parameters) {
        for (RParameter parameter : parameters) {
            if (name.equals(parameter.getName())) {
                return parameter;
            }
        }
        return null;
    }


    public static RType guessReturnValueTypeFromHelp(RHelpParser help) {
        if (help.getDocValue() == null) {
            return RUnknownType.INSTANCE;
        }
        return findType(help.getDocValue());
    }
}
