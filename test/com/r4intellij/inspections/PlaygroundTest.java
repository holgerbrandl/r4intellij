package com.r4intellij.inspections;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.util.Query;
import com.r4intellij.psi.api.RArgumentList;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RReferenceExpression;
import com.r4intellij.psi.references.RReferenceImpl;
import org.jetbrains.annotations.NotNull;

/**
 * @author Holger Brandl
 */
public class PlaygroundTest extends RInspectionTest {


    public void testPlayground() {
        CodeInsightTestFixture fixture = doExprTest("function() head(sdf)");

        RCallExpression childOfType = PsiTreeUtil.findChildOfType(fixture.getFile(), RCallExpression.class);
        RExpression argExpr = childOfType.getArgumentList().getExpressionList().get(0);

        //        RReferenceExpression argExpr = (RReferenceExpression) ((RAssignmentStatement) rExpression).getAssignedValue();

        RReferenceImpl reference = ((RReferenceExpression) argExpr).getReference();
        PsiElement resolvedRef = reference.resolve();
        assertNull(resolvedRef);
//        assertNotNull(resolvedRef);

        System.out.println(resolvedRef);
    }


    public void testPlayground2() {
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


