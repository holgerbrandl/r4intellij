package com.r4intellij.intentions;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import com.intellij.util.ArrayUtil;
import com.r4intellij.packages.RIndexCache;
import org.jetbrains.annotations.NotNull;

import java.io.File;

import static com.r4intellij.RTestCase.createSkeletonLibrary;
import static com.r4intellij.interpreter.RSkeletonGenerator.DEFAULT_PACKAGES;

/**
 * @author Holger Brandl
 */
public abstract class AbstractRIntentionTest extends CodeInsightFixtureTestCase {

    public static final String TEST_DATA_PATH = new File("testData").getAbsolutePath().replace(File.pathSeparatorChar, '/');


    @Override
    public void setUp() throws Exception {
        super.setUp();

        // note messing around with the fixture here is not a good idea, since the
        // CodeInsightFixtureTestCase shares some environment globally with the other tests


//        IdeaTestFixtureFactory factory = IdeaTestFixtureFactory.getFixtureFactory();
//        TestFixtureBuilder<IdeaProjectTestFixture> fixtureBuilder = factory.createLightFixtureBuilder();
//        final IdeaProjectTestFixture fixture = fixtureBuilder.getFixture();
//
//        myFixture = IdeaTestFixtureFactory.getFixtureFactory().createCodeInsightFixture(fixture, new LightTempDirTestFixtureImpl(true));
//        myFixture.setUp();

//        String intentionDataPath = super.getTestDataPath() + "/inspections/" + getClass().getSimpleName().replace("Test", "");
        String intentionDataPath = TEST_DATA_PATH + "/intentions/" + getClass().getSimpleName().replace("Test", "");
        myFixture.setTestDataPath(intentionDataPath);

        // inject stub index here for more reproducible testing
        RIndexCache.getTestInstance();
//        RIndexCache.getInstance().refreshIndex();

        // add base packages for testing
        createSkeletonLibrary(myFixture, ArrayUtil.toStringArray(DEFAULT_PACKAGES));
    }


    protected void doTest() {
        myFixture.configureByFile(getTestName(false) + ".before.R");

        // todo needed?
//        ApplicationManager.getApplication().runWriteAction(() -> intention.invoke(getProject(), getEditor(), getFile()));

        //
//        if(runAll){
//            final List<IntentionAction> intentions = myFixture.filterAvailableIntentions(getIntentionName());
//            intentions.forEach(i-> myFixture.launchAction(i));

//        }else {
        final IntentionAction intention = myFixture.findSingleIntention(getIntentionName());
        myFixture.launchAction(intention);
//        }

//        myFixture.doHighlighting();
//        val after = onLineStartingWith("check").inlays[0].getHintText()
//        assertThat(after).isNull()
//        PostprocessReformattingAspect.getInstance(project).doPostponedFormatting()

        myFixture.checkResultByFile(getTestName(false) + ".after.R");
    }


    protected void doExprTest(String before, String after) {
        myFixture.configureByText("a.R", before);

        final IntentionAction intention = myFixture.findSingleIntention(getIntentionName());
        myFixture.launchAction(intention);

        myFixture.checkResult(after);
    }


    @NotNull
    protected abstract String getIntentionName();
}
