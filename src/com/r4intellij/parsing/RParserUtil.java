package com.r4intellij.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.parser.GeneratedParserUtilBase;

/**
 * @author holgerbrandl
 */
public class RParserUtil extends GeneratedParserUtilBase {
    public static boolean parseEmptyExpression(PsiBuilder builder, int level) {
        PsiBuilder.Marker emptyMarker = builder.mark();
        emptyMarker.done(RElementTypes.R_EMPTY_EXPRESSION);
        return true;
    }
}
