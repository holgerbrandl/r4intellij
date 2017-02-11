package com.r4intellij.intentions;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.execution.ExecutionException;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import com.intellij.webcore.packaging.InstalledPackage;
import com.r4intellij.inspections.MissingPackageInspection;
import com.r4intellij.packages.RHelperUtil;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.packages.remote.RepoUtils;

import java.util.List;

/**
 * @author Holger Brandl
 */
public class InstallLibraryFixTest extends LightPlatformCodeInsightFixtureTestCase {

    // see     // from https://intellij-support.jetbrains.com/hc/en-us/community/posts/203365330-SetupJDKFix-fails-in-LightCodeInsightFixtureTestCase


    public void testPackageInstallationAction() throws Exception {
        // kill a package
        final String TEST_PACKAGE = "pals";

        try {
            RepoUtils.uninstallPackage(new InstalledPackage(TEST_PACKAGE, null));
        } catch (ExecutionException e) {
            e.printStackTrace();
            fail("could not uninstall package");
        }


        RPackageService.getInstance().refreshIndex("pals");
//        PsiFile psiFile = myFixture.configureByFile("...");

        myFixture.enableInspections(MissingPackageInspection.class);
        PsiFile psiFile = myFixture.configureByText("a.R", "require(pals)");
//        ensureIndexesUpToDate(myFixture.getProject());
//        myFixture.testHighlighting(false, false, false);
        List<IntentionAction> quickFixes = myFixture.getAllQuickFixes();

        myFixture.launchAction(assertOneElement(quickFixes));
        Thread.sleep(30000); // what a mess!!
//        assertSameLines(expected, psiFile.getText());


        // see org.jetbrains.plugins.groovy.intentions.GroovyConvertJUnitIntentionTest

        assertTrue(RHelperUtil.runCommand("require(pals) ==TRUE").contains("TRUE"));
    }


//    public void testTokenSimple2() throws Exception {
//        doTest("inline = 8\ninline + inline", "8 + 8");
//    }
//
//    private void doTest(/*@Language("R")*/ String text, /*@Language("R")*/ String expected) {
//        PsiFile file = myFixture.configureByText("a.r", text);
//
//        RAssignmentStatement rule = PsiTreeUtil.getChildOfType(file, RAssignmentStatement.class);
//        assertNotNull(rule);
//
//        new InlineAssignmentProcessor(rule, getProject(), null, false).run();
//        assertSameLines(expected, file.getText());
//    }


//    @SuppressWarnings("unchecked")
//    public void testBla() throws Exception {
//        myFixture.configureByFile("...");
//
//        List<IntentionAction> quickFixes = myFixture.getAllQuickFixes();
//        for (IntentionAction intention : quickFixes) {
//            myFixture.launchAction(intention);
//        }
//    }
}
