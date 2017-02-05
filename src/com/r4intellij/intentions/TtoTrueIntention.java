package com.r4intellij.intentions;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.psi.RElementFactory;
import com.r4intellij.psi.api.RLogicalLiteralExpression;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Objects;

/**
 * @author Holger Brandl
 */
public class TtoTrueIntention extends AbstractRIntention {

    @Override
    @NotNull
    public PsiElementPredicate getElementPredicate() {
        return new MergeElseIfPredicate();
    }


    @Override
    public void processIntention(@NotNull PsiElement element, @NotNull Project project, Editor editor)
            throws IncorrectOperationException {
//        final GrIfStatement parentStatement = (GrIfStatement) element;
//        GrBlockStatement elseBlockStatement = (GrBlockStatement) parentStatement.getElseBranch();
//        assert elseBlockStatement != null;
//        final GrOpenBlock elseBranch = elseBlockStatement.getBlock();
//        final GrStatement elseBranchContents = elseBranch.getStatements()[0];
//        PsiImplUtil.replaceStatement("if(" +
//                parentStatement.getCondition().getText() +
//                ")" +
//                parentStatement.getThenBranch().getText() +
//                "else " +
//                elseBranchContents.getText(), parentStatement);

        if (Objects.equals(element.getText(), "T")) {
            element.replace(RElementFactory.createLeafFromText(element.getProject(), "TRUE"));
        }
    }


    @Override
    @NotNull
    public String getText() {
        return "Convert T/F to TRUE and FALSE";
    }


    @Override
    @NotNull
    public String getFamilyName() {
        return "R best practices";
    }


    private static class MergeElseIfPredicate implements PsiElementPredicate {

        public boolean satisfiedBy(PsiElement element) {
            // todo what's the difference with RBooleanLiteralExpression??
            return PsiTreeUtil.getParentOfType(element, RLogicalLiteralExpression.class) != null && Arrays.asList("T", "T").contains(element.getText());

        }
    }

//    private static class MergeElseIfPredicate implements PsiElementPredicate {
//
//        public boolean satisfiedBy(PsiElement element) {
//            if (!(element instanceof PsiJavaToken)) {
//                return false;
//            }
//            @NonNls final String text = element.getText();
//            if (!PsiKeyword.ELSE.equals(text)) {
//                return false;
//            }
//            final PsiJavaToken token = (PsiJavaToken) element;
//            final PsiElement parent = token.getParent();
//            if (!(parent instanceof PsiIfStatement)) {
//                return false;
//            }
//            final PsiIfStatement ifStatement = (PsiIfStatement) parent;
//            if (ErrorUtil.containsError(ifStatement)) {
//                return false;
//            }
//            final PsiStatement thenBranch = ifStatement.getThenBranch();
//            final PsiStatement elseBranch = ifStatement.getElseBranch();
//            if (thenBranch == null) {
//                return false;
//            }
//            if (!(elseBranch instanceof PsiBlockStatement)) {
//                return false;
//            }
//            final PsiCodeBlock block =
//                    ((PsiBlockStatement) elseBranch).getCodeBlock();
//            final PsiStatement[] statements = block.getStatements();
//            return statements.length == 1 &&
//                    statements[0] instanceof PsiIfStatement;
//        }
//    }
}