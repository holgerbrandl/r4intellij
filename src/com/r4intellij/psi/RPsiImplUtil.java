package com.r4intellij.psi;

import com.google.common.base.CharMatcher;
import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RElementGenerator;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.references.ROperatorReference;
import com.r4intellij.psi.references.RReferenceImpl;
import com.r4intellij.typing.types.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static com.r4intellij.parsing.RElementTypes.*;

/**
 * @author Alefas
 * @author holgerbrandl
 * @since 27/01/15.
 */
public class RPsiImplUtil {
    public static final TokenSet LEFT_ASSIGNMENTS = TokenSet.create(
            R_LEFT_ASSIGN, R_LEFT_COMPLEX_ASSIGN);

    public static final TokenSet RIGHT_ASSIGNMENTS = TokenSet.create(
            R_RIGHT_ASSIGN, R_RIGHT_COMPLEX_ASSIGN);

    public static final TokenSet RESERVED_WORDS = TokenSet.create(
            R_IF, R_ELSE, R_REPEAT,
            R_WHILE, R_FUNCTION, R_FOR,
            R_IN, R_NEXT, R_BREAK);
    public static final TokenSet OPERATORS = TokenSet.create(
            R_MINUS, R_PLUS, R_NOT, R_TILDE, R_HELP,
            R_COLON, R_MULT, R_DIV, R_EXP,
            R_INFIX_OP, R_LT, R_GT, R_EQEQ, R_GE,
            R_LE, R_AND, R_ANDAND, R_OR, R_OROR,
            R_LEFT_ASSIGN, R_RIGHT_ASSIGN, R_LIST_SUBSET, R_AT);

    public static final TokenSet BUILTIN_CONSTANTS = TokenSet.create(
            R_NA_LITERAL, R_NA_LITERAL, R_BOUNDARY_LITERAL, R_BOOLEAN_LITERAL, R_NULL_LITERAL);

    public static String getName(ROperator binaryOperator) {
        return binaryOperator.getText();
    }


    public static ROperatorReference getReference(ROperator binaryOperator) {
        return new ROperatorReference(binaryOperator);
    }


    public static boolean isLeft(RAssignmentStatement assignment) {
        final ASTNode operator = assignment.getNode().findChildByType(LEFT_ASSIGNMENTS);
        return operator != null;
    }


    public static boolean isEqual(RAssignmentStatement assignment) {
        final ASTNode operator = assignment.getNode().findChildByType(R_EQ);
        return operator != null;
    }


    public static boolean isRight(RAssignmentStatement assignment) {
        final ASTNode operator = assignment.getNode().findChildByType(RIGHT_ASSIGNMENTS);
        return operator != null;
    }


    public static RPsiElement getAssignedValue(RAssignmentStatement assignment) {
        PsiElement child;
        if (!assignment.isRight()) {
            child = assignment.getLastChild();
            while (child != null && !(child instanceof RExpression)) {
                if (child instanceof PsiErrorElement)
                    return null; // incomplete assignment operator can't be analyzed properly, bail out.
                child = child.getPrevSibling();
            }
        } else {
            child = assignment.getFirstChild();
            while (child != null && !(child instanceof RExpression)) {
                if (child instanceof PsiErrorElement)
                    return null; // incomplete assignment operator can't be analyzed properly, bail out.
                child = child.getNextSibling();
            }
        }
        return (RPsiElement) child;
    }


    public static PsiElement getAssignee(RAssignmentStatement assignment) {
        final ASTNode node = assignment.getNode();
        PsiElement child;
        if (!assignment.isRight()) {
            child = assignment.getFirstChild();
            while (child != null && !(child instanceof RExpression)) {
                if (child instanceof PsiErrorElement)
                    return null; // incomplete assignment operator can't be analyzed properly, bail out.
                child = child.getPrevSibling();
            }
        } else {
            child = assignment.getLastChild();
            while (child != null && !(child instanceof RExpression)) {
                if (child instanceof PsiErrorElement)
                    return null; // incomplete assignment operator can't be analyzed properly, bail out.
                child = child.getNextSibling();
            }
        }
        return child;
    }


    public static PsiElement setName(RAssignmentStatement assignment, String name) {
        ASTNode nameNode = assignment.getNameNode();
        if (nameNode == null) {
            return assignment;
        }
        final ASTNode oldNameIdentifier = nameNode.findChildByType(R_IDENTIFIER);
        if (oldNameIdentifier != null) {
            final PsiFile dummyFile = RElementGenerator.createDummyFile(name, false, assignment.getProject());
            ASTNode identifier = dummyFile.getNode().getFirstChildNode().findChildByType(R_IDENTIFIER);
            if (identifier != null) {
                nameNode.replaceChild(oldNameIdentifier, identifier);
            }
        }
        return assignment;
    }


    public static String getName(RAssignmentStatement assignment) {
        final ASTNode node = assignment.getNameNode();

        // for member assignments return the expression reference, e.g foo$bar = 1 # return foo
        if (assignment.getAssignee() instanceof RMemberExpression) {
            return ((RMemberExpression) assignment.getAssignee()).getExpression().getText();
        }

        // for operator definitions we strip leading or tailing quotes
        CharMatcher charMatcher = CharMatcher.anyOf("`\"'");
        String assigneeText = assignment.getAssignee().getText();

        if (assignment.getAssignedValue() instanceof RFunctionExpression &&
                charMatcher.matches(assigneeText.charAt(0))) {
            return charMatcher.trimFrom(assigneeText);
        }

        return node != null ? node.getText() : null;
    }


    public static ASTNode getNameNode(RAssignmentStatement assignment) {
        PsiElement assignee = assignment.getAssignee();
        return assignee == null ? null : assignee.getNode();
    }


    public static ASTNode getNameNode(RParameter parameter) {
        PsiElement identifier = parameter.getIdentifier();
        return identifier == null ? null : identifier.getNode();
    }


    public static String getName(RParameter parameter) {
        ASTNode node = parameter.getNameNode();
        return node == null ? null : node.getText();
    }


    public static PsiElement setName(RParameter parameter, String name) {
        final ASTNode oldNameIdentifier = parameter.getNameNode();
        if (oldNameIdentifier != null) {
            final PsiFile dummyFile = RElementGenerator.createDummyFile(name, false, parameter.getProject());
            ASTNode identifier = dummyFile.getNode().getFirstChildNode().findChildByType(R_IDENTIFIER);
            if (identifier != null) {
                parameter.getNode().replaceChild(oldNameIdentifier, identifier);
            }
        }
        return parameter;
    }


    public static RReferenceImpl getReference(RReferenceExpression referenceExpression) {
        final PsiElement nextElement = PsiTreeUtil.skipSiblingsForward(referenceExpression, PsiWhiteSpace.class);
        if (nextElement != null && LEFT_ASSIGNMENTS.contains(nextElement.getNode().getElementType())) return null;

        final PsiElement prevElement = PsiTreeUtil.skipSiblingsBackward(referenceExpression, PsiWhiteSpace.class);
        if (prevElement != null && RIGHT_ASSIGNMENTS.contains(prevElement.getNode().getElementType())) return null;

        return new RReferenceImpl(referenceExpression);
    }


    public static String getTag(RMemberExpression memberExpression) {
        PsiElement identifier = memberExpression.getIdentifier();
        if (identifier != null) {
            return identifier.getText();
        }
        PsiElement name = memberExpression.getString();
        if (name != null) {
            return name.getText().substring(1, name.getText().length() - 1);
        }
        return "...";
    }


    public static String getTag(RAtExpression atExpression) {
        PsiElement identifier = atExpression.getIdentifier();
        if (identifier != null) {
            return identifier.getText();
        }
        PsiElement name = atExpression.getString();
        if (name != null) {
            return name.getText().substring(1, name.getText().length() - 1);
        }
        return "...";
    }


    @Nullable
    public static String getDocStringValue(@NotNull final RFunctionExpression functionExpression) {  //TODO: make stub-aware
        String roxyComment = StringUtil.nullize(RPsiImplUtil.getRoxygenComment(functionExpression));
        if (roxyComment != null) {
            return roxyComment;
        }

        final RBlockExpression blockExpression = PsiTreeUtil.findChildOfType(functionExpression, RBlockExpression.class);
        if (blockExpression == null) return null;

        final List<PsiComment> comments = new ArrayList<PsiComment>();
        for (PsiElement sibling = blockExpression.getFirstChild(); sibling != null && !(sibling instanceof RExpression);
             sibling = sibling.getNextSibling()) {
            if (sibling instanceof PsiComment) {
                comments.add((PsiComment) sibling);
            }
        }

        if (!comments.isEmpty()) {
            List<String> stringifiedComments = comments.stream().
                    map(c -> trimCommentPrefix(c.getText())).
                    collect(Collectors.toList());

            // todo why not using dummy RHelpParser here?
            return Joiner.on("\n").join(stringifiedComments);
        } else {
            return null;
        }
    }


    @Nullable
    public static String getRoxygenComment(RFunctionExpression functionExpression) {
        List<String> detectObjectDocs = Lists.newArrayList();


        // traverse back before the function def and collect roxygen comment lines
        int newLineCounter = 0;
        for (PsiElement sibling = functionExpression.getParent().getPrevSibling(); sibling != null && (sibling instanceof PsiComment || sibling.getText().equals("\n")); ) {

            if (sibling instanceof PsiComment && sibling.getText().startsWith("#'")) {
                detectObjectDocs.add(trimCommentPrefix(sibling.getText()).trim());
                newLineCounter = 0;
            } else {
                newLineCounter++;

                if (newLineCounter > 2) {
                    break;
                }
            }
            sibling = sibling.getPrevSibling();
        }

        return Joiner.on("<br>").join(Lists.reverse(detectObjectDocs));
    }


    @NotNull
    public static String trimCommentPrefix(String text) {
        String string = text;

        string = StringUtil.trimStart(string, "#'"); // roxygen  style comment trimming
        string = StringUtil.trimStart(string, "#'"); // regular comment style comment trimming
        return string;
    }


    @Nullable
    public static String getNamespace(RReferenceExpression referenceExpression) {
        final String text = referenceExpression.getText();
        final int namespaceIndex = text.indexOf("::");
        if (namespaceIndex > 0) {
            return text.substring(0, namespaceIndex);
        }
        return null;
    }


    public static String getName(RReferenceExpression referenceExpression) {
        final String text = referenceExpression.getText();
        final int namespaceIndex = text.indexOf("::");
        if (namespaceIndex > 0) {
            return text.substring(namespaceIndex + 2);
        }

        // also support operator assignments here
        if (referenceExpression.getParent() instanceof RAssignmentStatement) {
            CharMatcher charMatcher = CharMatcher.anyOf("`\"'");
            return charMatcher.trimFrom(text);
        }

        // since any r method can be called with surrunding backticks we discard them here since they are not part of the stub index
        CharMatcher charMatcher = CharMatcher.anyOf("`");
        return charMatcher.trimFrom(text);

//        return text;
    }


    // operator expressions


    public static boolean isBinary(ROperatorExpression expr) {
        return expr.getChildren().length == 3;
    }


    @Nullable
    public static ROperator getOperator(ROperatorExpression expr) {
        return PsiTreeUtil.getChildOfType(expr, ROperator.class);
    }


    public static RExpression getLeftExpr(ROperatorExpression expr) {
        // not correct because of whitespace psi elements

//        if (expr.getChildren().length == 2) {
//            throw new AssertionError("unary operator expression has no left side argument");
//        }
        //redo with
//        Collection<RPsiElement> psiChildren = PsiTreeUtil.findChildrenOfAnyType(expr, RExpression.class, ROperator.class);

        PsiElement psiElement = expr.getChildren()[0];
        if (psiElement instanceof RExpression) {
            return (RExpression) psiElement;
        }

        return null;
    }


    public static RExpression getRightExpr(ROperatorExpression expr) {
        RExpression[] expressions = PsiTreeUtil.getChildrenOfType(expr, RExpression.class);

        if (expressions == null || expressions.length != 2) return null;

        return expressions[1];
    }


    public static boolean isTrue(RBooleanLiteral boolExpr) {
        ASTNode node = boolExpr.getNode();
        return node.findChildByType(TokenSet.create(R_TRUE, R_T)) != null;
    }


    public static boolean isFalse(RBooleanLiteral boolExpr) {
        return !isTrue(boolExpr);
    }


    public static RType getType(RNaLiteral na) {
        if (na.getNa() != null) {
            return RLogicalType.INSTANCE;
        }
        if (na.getNaCharacter() != null) {
            return RCharacterType.INSTANCE;
        }
        if (na.getNaComplex() != null) {
            return RComplexType.INSTANCE;
        }
        if (na.getNaInteger() != null) {
            return RIntegerType.INSTANCE;
        }
        if (na.getNaReal() != null) {
            return RNumericType.INSTANCE;
        }

        return RUnknownType.INSTANCE;
    }


    /**
     * Returns method name if function expression is not an anoymous function definition.
     */
    @Nullable
    public static String getName(@NotNull RFunctionExpression functionExpression) {
        if (functionExpression.getParent() instanceof RAssignmentStatement) {
            return ((RAssignmentStatementImpl) functionExpression.getParent()).getName();
        }

        return null;
    }
}
