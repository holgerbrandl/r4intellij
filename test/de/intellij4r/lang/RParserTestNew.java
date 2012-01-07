/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package de.intellij4r.lang;

import com.intellij.openapi.fileEditor.impl.LoadTextUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.testFramework.LightVirtualFile;
import com.r4intellij.lang.parser.RParserDefinition;

import java.io.IOException;


/**
 * @author gregsh
 */
public class RParserTestNew extends RParsingTestCase {

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

            text = "function(x,...) { x; };";
            text = "{ x }\n";
            text = "for(a in dff){ x };\n # bla bla\n foo <- 232\n";
            text = ".ls.objects <- function (pos = 1)\n pos\n";

            text = StringUtil.convertLineSeparators(text);

            myFile = createPsiFile(fileName.replace(".R", ".txt"), text);
//          ensureParsed(myFile);
            assertEquals("light virtual file text mismatch", text, ((LightVirtualFile) myFile.getVirtualFile()).getContent().toString());
            assertEquals("virtual file text mismatch", text, LoadTextUtil.loadText(myFile.getVirtualFile()));
            assertEquals("doc text mismatch", text, myFile.getViewProvider().getDocument().getText());
            assertEquals("psi text mismatch", text, myFile.getText());

            System.out.println(DebugUtil.psiToString(myFile, true, false));
//            if (true) {
//                checkResult(name + ".txt", myFile);
//            } else {
//                toParseTreeText(myFile, skipSpaces(), includeRanges());
//            }
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
