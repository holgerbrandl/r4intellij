package com.r4intellij.misc;

import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.rt.execution.junit.FileComparisonFailure;
import com.r4intellij.RTestCase;
import com.r4intellij.documentation.RDocumentationProvider;
import com.r4intellij.psi.RReferenceExpressionImpl;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RExpression;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author Holger Brandl
 */
public class DocumentationProviderTest extends RTestCase {

    @Override
    public void setUp() throws Exception {
        super.setUp();

        RDocumentationProvider.startHelpServer(5678);
    }


    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/misc/" + getClass().getSimpleName().replace("Test", "");
    }


    public void testDocForRexportedSymbol() throws IOException {
        createSkeletonLibrary("dplyr");

        myFixture.configureByText("a.R", "require(dplyr); glimps<caret>e"); // TODO move caret to end
        PsiElement refExpr = PsiUtilBase.getElementAtCaret(myFixture.getEditor());
        PsiElement resolved = ((RReferenceExpressionImpl) refExpr.getParent()).getReference().resolve();
        String doc = new RDocumentationProvider().generateDoc(resolved, refExpr);

        assertTrue(doc.contains("This is like a transposed version of print"));
    }



    public void testTidyrGroupBy() throws IOException {
        myFixture.configureByText("a.R", "dplyr::group_by(iris)");

        RCallExpression callExpression = PsiTreeUtil.findChildOfType(myFixture.getFile(), RCallExpression.class);
        RExpression funExpr = callExpression.getExpression();
        String doc = new RDocumentationProvider().
                generateDoc(funExpr, ((RReferenceExpressionImpl) funExpr).getIdentifier());

        // assert that doc is as expcted
        compareWIthTestData(doc);
    }


    public void testHelpForSpecialConstants() throws IOException {
        // special constants
        List<String> constants = Arrays.asList("NULL", "NA", "Inf", "NaN", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_",
                "if", "else", "repeat", "while", "function", "for", "next", "break");

        for (String specialConstant : constants) {
            System.err.println("testing " + specialConstant);
            myFixture.configureByText("a.R", specialConstant);

//            RPsiElement refExpr = PsiTreeUtil.findChildOfType(myFixture.getFile(), RPsiElement.class);
            PsiElement refExpr = PsiUtilBase.getElementAtCaret(myFixture.getEditor());
//            RPsiElement refExpr = (RPsiElement) myFixture.getElementAtCaret();
            String doc = new RDocumentationProvider().generateDoc(refExpr, refExpr);

            assertTrue(doc.contains(specialConstant) && doc.contains("Description"));
        }
    }


    public void testHead() throws IOException {
        myFixture.configureByText("a.R", "head(iris)");

        PsiElement refExpr = PsiUtilBase.getElementAtCaret(myFixture.getEditor());
        PsiElement resolved = ((RReferenceExpressionImpl) refExpr.getParent()).getReference().resolve();

        String doc = new RDocumentationProvider().generateDoc(resolved, refExpr);

        // assert that doc is as expected
        compareWIthTestData(doc);
    }


    private void compareWIthTestData(String doc) throws IOException {
        File testDataFile = new File(getTestDataPath(), getName().replaceFirst("test", "") + ".txt");
        if (!testDataFile.exists()) {
            // http://stackoverflow.com/questions/22859453/what-is-the-simplest-way-to-write-a-text-file-in-java
            Files.write(Paths.get(testDataFile.getAbsolutePath()), doc.getBytes());
        }

        // http://www.adam-bien.com/roller/abien/entry/java_8_reading_a_file
        String expected = new String(Files.readAllBytes(Paths.get(testDataFile.getAbsolutePath())));


        if (!Objects.equals(doc, expected)) {
            throw new FileComparisonFailure("documenation mismatch", expected, doc, testDataFile.getAbsolutePath());
        }
    }
}
