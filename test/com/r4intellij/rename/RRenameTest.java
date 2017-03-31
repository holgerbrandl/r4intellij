package com.r4intellij.rename;

import com.r4intellij.RTestCase;

import static com.r4intellij.RFileType.DOT_R_EXTENSION;

public class RRenameTest extends RTestCase {

    private void doTestWithProject(String newName) {
        myFixture.configureByFile("rename/" + getTestName(true) + DOT_R_EXTENSION);
        myFixture.renameElementAtCaret(newName);
        myFixture.checkResultByFile("rename/" + getTestName(true) + ".after.R", true);
    }


    public void testRenameFunction() {
        doTestWithProject("test_function1");
    }


    public void testRenameParameter() {
        doTestWithProject("x1");
    }
}
