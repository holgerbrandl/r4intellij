package com.r4intellij.typing;

import com.intellij.psi.PsiElement;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.parsing.RParserDefinition;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RParameter;
import com.r4intellij.typing.types.RType;

import java.util.ArrayList;
import java.util.List;

public class DocStringUtil {
  public static final String COMMENT_SYMBOL = "##";

  public static List<Substring> getDocStringLines(RAssignmentStatement statement) {
    List<Substring> lines = new ArrayList<Substring>();
    PsiElement comment = getNextComment(statement);
    while (comment != null && comment.getText().startsWith(COMMENT_SYMBOL)) {
      lines.add(processComment(comment.getText()));
      comment = getNextComment(comment);
    }
    return lines;
  }

  private static Substring processComment(String text) {
    Substring substring = new Substring(text);
    return substring.substring(COMMENT_SYMBOL.length()).trim();
  }

  private static PsiElement getNextComment(PsiElement element) {
    PsiElement nextLine = element.getPrevSibling();
    if (nextLine == null || nextLine.getNode().getElementType() != RElementTypes.THE_R_NL) {
      return null;
    }
    PsiElement comment = nextLine.getPrevSibling();
    return isComment(comment) ? comment : null;
  }

  private static boolean isComment(PsiElement comment) {
    return comment != null && comment.getNode().getElementType() == RParserDefinition.END_OF_LINE_COMMENT;
  }

  public static String generateTypeAnnotation(RParameter parameter, RType type) {
    return "## @type " + parameter.getName() + " : " + type.toString() + "\n";
  }
}
