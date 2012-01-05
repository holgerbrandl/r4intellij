/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package de.intellij4r.lang;

import com.intellij.openapi.fileEditor.impl.LoadTextUtil;
import com.intellij.testFramework.LightVirtualFile;
import com.intellij.testFramework.ParsingTestCase;
import com.r4intellij.lang.parser.RParserDefinition;

import java.io.IOException;


/**
 * @author gregsh
 */
public class RParserTestNew extends ParsingTestCase {

    public RParserTestNew() {
        super("parser", "R", new RParserDefinition());
    }

    @Override
    protected String getTestDataPath() {
        return "misc/testData";
    }

    public void testSimpleTest1() {
        doTest(true);
    }

    public void testSimpleTestDebug() {
//    doTest(true);
        String name = getTestName(false);
        try {
            String fileName = "simpleTest2.R";
            String text = loadFile(fileName);
            myFile = createPsiFile(fileName.replace(".R", ".txt"), text);
//          ensureParsed(myFile);
            assertEquals("light virtual file text mismatch", text, ((LightVirtualFile) myFile.getVirtualFile()).getContent().toString());
            assertEquals("virtual file text mismatch", text, LoadTextUtil.loadText(myFile.getVirtualFile()));
            assertEquals("doc text mismatch", text, myFile.getViewProvider().getDocument().getText());
            assertEquals("psi text mismatch", text, myFile.getText());
            if (true) {
                checkResult(name + ".txt", myFile);
            } else {
                toParseTreeText(myFile, skipSpaces(), includeRanges());
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected boolean skipSpaces() {
        return true;
    }

    public void testBrokenAttr() {
        doTest(true);
    }

    public void testBrokenEverything() {
        doTest(true);
    }

    public void testAlternativeSyntax() {
        doTest(true);
    }

    public void testExternalExpression() {
        doTest(true);
    }
}
