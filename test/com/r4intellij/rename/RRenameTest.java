package com.r4intellij.rename;

import com.r4intellij.RTestCase;

public class RRenameTest extends RTestCase {

    private void doTestWithProject(String newName) {
        myFixture.configureByFile("rename/" + getTestName(true) + ".R");
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
