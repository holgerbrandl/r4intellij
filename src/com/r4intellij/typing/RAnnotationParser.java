package com.r4intellij.typing;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.r4intellij.RElementGenerator;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.typing.types.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RAnnotationParser {

  public static final String TAG_PREFIX = "@";
  public static final Pattern SPACE_PATTERN = Pattern.compile("\\s");
  public static final String TYPE_TAG = "type";
  public static final String RETURN_TAG = "return";
  public static final String RULE_TAG = "rule";
  public static final Pattern COLON_PATTERN = Pattern.compile(":");
  public static final Pattern ARROW_PATTERN = Pattern.compile("->");
  public static final Pattern COMMA_PATTERN = Pattern.compile(",");
  public static final Pattern EQUALS_PATTERN = Pattern.compile("=");
  public static final Pattern MAX_PATTERN = Pattern.compile("max\\((.*)\\)");
  public static final Pattern ERROR_PATTERN = Pattern.compile("error\\((.*)\\)");
  public static final Pattern S3_PATTERN = Pattern.compile("([^\\[\\]]*)\\[(.*)\\]");
  private static final String OPTIONAL_TAG = "optional";
  private RFunctionType myType;

  public RAnnotationParser(RFunctionType type) {
    myType = type;
  }

  public void interpretLine(Substring line) {
    if (!line.startsWith(TAG_PREFIX)) {
      return;
    }
    line = line.substring(TAG_PREFIX.length());
    List<Substring> split = line.split(SPACE_PATTERN);
    Substring tagName = split.get(0);
    if (tagName != null) {
      parseTag(tagName, line.substring(tagName.length()).trim());
    }
  }

  @SuppressWarnings("EqualsBetweenInconvertibleTypes")
  private void parseTag(Substring tagName, Substring line) {
    if (tagName.equals(TYPE_TAG)) {
      parseType(line);
      return;
    }
    if (tagName.equals(RETURN_TAG)) {
      parseReturn(line);
      return;
    }
    if (tagName.equals(RULE_TAG)) {
      parseRule(line);
    }

    if (tagName.equals(OPTIONAL_TAG)) {
      parseOptional(line);
    }
  }

  private void parseOptional(Substring line) {
    List<Substring> optionalParams = line.split(COMMA_PATTERN);
    for (Substring param : optionalParams) {
      myType.setOptional(param.getValue().trim());
    }
  }

  private void parseRule(Substring line) {
    List<Substring> split = line.split(ARROW_PATTERN);
    if (split.size() != 2) {
      return;
    }
    Substring parameters = split.get(0).trim();
    Substring returnType = split.get(1).trim();
    RFunctionRule rule = new RFunctionRule(findType(returnType));
    if (parameters.startsWith("(") && parameters.getValue().endsWith(")")) {
      parseParameters(parameters.substring(1, parameters.length() - 1), rule);
    }
    myType.addRule(rule);
  }

  private RType findType(Substring typeSubstring) {
    String typeName = typeSubstring.trim().getValue();
    Matcher s3Matcher = S3_PATTERN.matcher(typeName);
    if (s3Matcher.matches()) {
      Substring baseTypeSubstring = new Substring(s3Matcher.group(1));
      Substring s3ClassesSubstring = new Substring(s3Matcher.group(2));
      RType baseType = findType(baseTypeSubstring);
      List<String> s3Classes = new ArrayList<String>();
      for (Substring s3Class : s3ClassesSubstring.split(COMMA_PATTERN)) {
        s3Classes.add(s3Class.trim().toString());
      }
      baseType = baseType.replaceS3Types(s3Classes);
      return baseType;
    }
    Matcher maxMatcher = MAX_PATTERN.matcher(typeName);
    if (maxMatcher.matches()) {
      Substring types = new Substring(maxMatcher.group(1));
      List<RType> typesList = new ArrayList<RType>();
      for (Substring type : types.split(COMMA_PATTERN)) {
        typesList.add(findType(type));
      }
      return new RMaxType(typesList);
    }
    Matcher errorMatcher = ERROR_PATTERN.matcher(typeName);
    if (errorMatcher.matches()) {
      return new RErrorType(errorMatcher.group(1));
    }
    RType type = createType(typeName);
    return type != null ? type : new RTypeVariable(typeName);
  }

  private void parseParameters(Substring line, RFunctionRule rule) {
    List<Substring> split = line.split(COMMA_PATTERN);
    for (Substring parameterDescr : split) {
      parseParameterRule(parameterDescr, rule);
    }
  }

  private void parseParameterRule(Substring descr, RFunctionRule rule) {
    List<Substring> split = descr.split(COLON_PATTERN);
    if (split.size() == 1) {
      split = descr.split(EQUALS_PATTERN);
      if (split.size() == 2) {
        PsiFile file =
          RElementGenerator.createDummyFile(split.get(1).trim().getValue(), false, myType.getFunctionExpression().getProject());
        PsiElement child = file.getFirstChild();
        if (child instanceof RExpression) {
          rule.addParameter(split.get(0).trim().getValue(), null, (RExpression)child);
        }
      }
    }
    else if (split.size() == 2) {
      String name = split.get(0).trim().getValue();
      Substring typeAndMaybeValue = split.get(1);
      final Substring typeSubstring;
      RExpression value = null;
      split = typeAndMaybeValue.split(EQUALS_PATTERN);
      if (split.size() == 1) {
        typeSubstring = typeAndMaybeValue;
      }
      else {
        PsiFile file =
          RElementGenerator.createDummyFile(split.get(1).trim().getValue(), false, myType.getFunctionExpression().getProject());
        PsiElement child = file.getFirstChild();
        if (child instanceof RExpression) {
          value = (RExpression)child;
        }
        typeSubstring = split.get(0);
      }
      RType type = findType(typeSubstring);
      // TODO: check that 'value' type matches 'type'
      rule.addParameter(name, type, value);
    }
  }

  private void parseReturn(Substring line) {
    String typeName = line.trim().getValue();
    RType type = createType(typeName);
    if (type != null && !RUnknownType.class.isInstance(type)) {
      myType.setReturnType(type);
    }
  }

  private void parseType(Substring line) {
    List<Substring> split = line.split(COLON_PATTERN);
    if (split.size() > 2) {
      return;
    }
    Substring parameterName = split.get(0).trim();
    Substring typeName = split.get(1).trim();
    RType type = createType(typeName.getValue());
    if (type != null && !RUnknownType.class.isInstance(type)) {
      myType.addParameterType(parameterName.getValue(), type);
    }
  }

  private RType createType(String typeName) {
    String[] typeNames = typeName.split("\\|");
    Set<RType> types = new HashSet<RType>();
    for (String name : typeNames) {
      RType type = RTypeProvider.findTypeByName(name.trim());
      if (type == null) {
        return null;
      }
      if (!RUnknownType.class.isInstance(type)) {
        types.add(type);
      }
    }
    return RUnionType.create(types);
  }
}
