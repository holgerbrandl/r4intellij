package com.r4intellij.inspections;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.util.Query;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.references.RReferenceImpl;
import org.jetbrains.annotations.NotNull;

/**
 * @author Holger Brandl
 */
public class PlaygroundTest extends RInspectionTest {


    public void testDummy() {
    }

    public void _testPlayground() {
        CodeInsightTestFixture fixture = doExprTest("function(usedArg) head(x=usedArg)");

        RCallExpression childOfType = PsiTreeUtil.findChildOfType(fixture.getFile(), RCallExpression.class);
        RExpression argExpr = childOfType.getArgumentList().getExpressionList().get(0);

        //        RReferenceExpression argExpr = (RReferenceExpression) ((RAssignmentStatement) rExpression).getAssignedValue();

//        RReferenceExpression reference = (RReferenceExpression)((RAssignmentStatement) argExpr).getAssignedValue();
        RReferenceExpression reference = (RReferenceExpression) ((RAssignmentStatement) argExpr).getAssignee();
        PsiElement resolvedRef = reference.getReference().resolve();

        System.out.println(resolvedRef);
    }


    public void _testPlayground2() {
        CodeInsightTestFixture fixture = doExprTest("function(sdf) head(x=sdf)");

        RArgumentList childOfType = PsiTreeUtil.findChildOfType(fixture.getFile(), RArgumentList.class);
        RExpression rExpression = childOfType.getExpressionList().get(0);
//
        System.out.println(rExpression.getText());
//
//        RReferenceExpression refExpr = (RReferenceExpression) ((RAssignmentStatement) rExpression).getAssignedValue();
        RReferenceExpression refExpr = (RReferenceExpression) rExpression;
        RReferenceImpl reference = refExpr.getReference();
        PsiElement resolvedRef = reference.resolve();
        Query<PsiReference> search = ReferencesSearch.search(resolvedRef);
        PsiReference first = search.findFirst();
        assertNotNull(first);


        assertNotNull(resolvedRef);
        System.out.println(resolvedRef);
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return MissingPackageInspection.class;
    }


    class DummyInspection extends RInspection {

    }
}


