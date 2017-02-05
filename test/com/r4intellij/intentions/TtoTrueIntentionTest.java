package com.r4intellij.intentions;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

/**
 * @author Holger Brandl
 */
public class TtoTrueIntentionTest extends CodeInsightFixtureTestCase {

    @Override
    public void setUp() throws Exception {
        super.setUp();
        // todo fixme
//        DartTestUtils.configureDartSdk(myModule, getTestRootDisposable(), true);
//        DartAnalysisServerService.getInstance(getProject()).serverReadyForRequest(getProject());
//        myFixture.setTestDataPath(DartTestUtils.BASE_TEST_DATA_PATH + getBasePath());
    }


    protected String getBasePath() {
        return "/analysisServer/intentions";
    }


    private void doTest(@NotNull final String intentionName) {
        myFixture.configureByFile(getTestName(false) + ".dart");
        final IntentionAction intention = myFixture.findSingleIntention(intentionName);

        ApplicationManager.getApplication().runWriteAction(() -> intention.invoke(getProject(), getEditor(), getFile()));

        myFixture.checkResultByFile(getTestName(false) + ".after.dart");
    }


    public void testIntroduceVariableNoSelection() throws Throwable {
        doTest("Assign value to new local variable");
    }

}
