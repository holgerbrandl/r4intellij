package com.r4intellij.misc;

import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.rt.execution.junit.FileComparisonFailure;
import com.r4intellij.RTestCase;
import com.r4intellij.documentation.RDocumentationProvider;
import com.r4intellij.psi.RReferenceExpressionImpl;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RReferenceExpression;

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
                "if", "else", "repeat", "while", "function", "for", "in", "next", "break");

        for (String specialConstant : constants) {
            myFixture.configureByText("a.R", specialConstant);

            RReferenceExpression refExpr = PsiTreeUtil.findChildOfType(myFixture.getFile(), RReferenceExpression.class);
            String doc = new RDocumentationProvider().generateDoc(refExpr, refExpr);

            assertTrue(doc.contains(specialConstant) && doc.contains("Description"));
        }
    }


    public void testHead() throws IOException {
        myFixture.configureByText("a.R", "head(iris)");

        RCallExpression callExpression = PsiTreeUtil.findChildOfType(myFixture.getFile(), RCallExpression.class);
        RExpression funExpr = callExpression.getExpression();
        String doc = new RDocumentationProvider().
                generateDoc(funExpr, ((RReferenceExpressionImpl) funExpr).getIdentifier());

        // assert that doc is as expcted
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
