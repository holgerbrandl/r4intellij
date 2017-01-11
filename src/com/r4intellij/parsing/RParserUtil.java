package com.r4intellij.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.parser.GeneratedParserUtilBase;

/**
 * @author Alefas
 * @since 23/12/14.
 */
public class RParserUtil extends GeneratedParserUtilBase {
  public static boolean parseEmptyExpression(PsiBuilder builder, int level) {
    PsiBuilder.Marker emptyMarker = builder.mark();
    emptyMarker.done(RElementTypes.THE_R_EMPTY_EXPRESSION);
    return true;
  }
}
