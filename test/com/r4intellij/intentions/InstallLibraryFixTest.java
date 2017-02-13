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

    // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/203365330-SetupJDKFix-fails-in-LightCodeInsightFixtureTestCase
    // alsp see org.jetbrains.plugins.groovy.intentions.GroovyConvertJUnitIntentionTest


    // TODO enable once UI disposal issue has been resolved
    public void _testPackageInstallationAction() throws Exception {
        // remove the package so that we can detect it as missing
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

        // not needed here
        // myFixture.testHighlighting(false, false, false);

        List<IntentionAction> quickFixes = myFixture.getAllQuickFixes();

        myFixture.launchAction(assertOneElement(quickFixes));
        Thread.sleep(30000); // what a mess!!

        assertTrue(RHelperUtil.runCommand("require(pals) ==TRUE").contains("TRUE"));
    }
}
