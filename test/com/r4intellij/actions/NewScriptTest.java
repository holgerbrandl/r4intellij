/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.actions;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.FileTypes;
import com.intellij.openapi.util.Computable;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiFile;
import com.r4intellij.RTestCase;
import org.junit.Assert;

public class NewScriptTest extends RTestCase {

    //    @Test
//    @DisplayName
    public void testSimpleCreate() throws Exception {
        PsiFile newScript = createNewRScript(findSrcDir(), "new_file.R", NewRScriptAction.DEFAULT_TEMPLATE_FILENAME);
        Assert.assertNotNull(newScript);
        Assert.assertEquals("new_file.R", newScript.getName());
        Assert.assertTrue(newScript.getText().contains("Created by"));
    }

//    @Test
//    public void testInvalidTemplate() throws Exception {
//        try {
//            createNewBashFile(findSrcDir(), "new_file", "not-existing-template");
//            Assert.fail("Template must not exist");
//        } catch (Throwable e) {
//            Assert.assertTrue(e.getMessage().contains("Template not found: not-existing-template"));
//        }
//    }


    private PsiDirectory findSrcDir() {
        return myFixture.configureByText(FileTypes.PLAIN_TEXT, "dummy content").getContainingDirectory();
    }


    private static PsiFile createNewRScript(final PsiDirectory srcDir, final String fileName, final String templateName) {
        return ApplicationManager.getApplication().runWriteAction((Computable<PsiFile>) () ->
                NewRScriptAction.createFromTemplate(srcDir, fileName, templateName));
    }
}