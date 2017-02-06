package com.r4intellij.intentions;

import org.jetbrains.annotations.NotNull;


/**
 * @author Holger Brandl
 */
public class TtoTrueIntentionTest extends AbstractRIntentionTest {

    // example /Users/brandl/projects/jb/intellij-community/java/java-tests/testData/codeInsight/addJavadoc/afterClass.java
    // com/intellij/codeInsight/intention/AddJavadocIntentionTest.java

    @Override
    public void setUp() throws Exception {
        super.setUp();
        // todo fixme
//        DartTestUtils.configureDartSdk(myModule, getTestRootDisposable(), true);
//        DartAnalysisServerService.getInstance(getProject()).serverReadyForRequest(getProject());
//        myFixture.setTestDataPath(DartTestUtils.BASE_TEST_DATA_PATH + getBasePath());
    }


    public void testBooleanAssignment() throws Throwable {
        doExprTest("foo = <caret>T", "foo = TRUE<caret>");
    }


    public void testLoopCheck() throws Throwable {
        doTest();
    }


    @Override
    @NotNull
    protected String getIntentionName() {
        return new TtoTrueIntention().getText();
    }
}
