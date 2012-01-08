/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package de.intellij4r.lang;

import com.intellij.psi.impl.DebugUtil;
import com.r4intellij.lang.parser.RParserDefinition;

import java.io.IOException;


/**
 * @author brandl
 */
public class RParserTest extends RParsingTestCase {


    public void testPrintPsiTree() throws IOException {
        String fileName = "ComplexScript.R";
        String text = loadFile(fileName);

//            text = "function(x,...) { \ntt; };";
////            text = "{ x }\n";
////            text = "for(a in dff){ x };\n # bla bla\n foo <- 232\n";
////            text = ".ls.objects <- function (pos = 1)\n pos\n";
//            text = "ggplot()\n";
//            text = "{\n" +
////                    "# A\n" +
//                    "}\n";
//
//            // RECURSION LEVEL TEST
//            text = "xout_pdomains <- function(df_with_segs_and_seq){\n" +
//                    "\tadply(df_with_segs_and_seq, 1, splat(function(Sequence, Segmentation, ...){\n" +
//                    "\t\treturn(c(prion_xout_sequence=paste(split_sequence)))\n" +
//                    "\t}), .progress=\"text\")\n" +
//                    "}\n";
//            text = "xout_pdomains <-function(df_with_segs_and_seq){\n" +
//                    "\tadply(df_with_segs_and_seq, 1, splat(function(Sequence, Segmentation, ...){\n" +
//                    "\t\treturn(c(prion_xout_sequence=paste(split_sequence)))\n" +
//                    "\t}), .progress=\"text\")\n" +
//                    "}\n";        text = StringUtil.convertLineSeparators(text);
        myFile = createPsiFile("tmpPsiFile.txt", text);
        System.out.println(DebugUtil.psiToString(myFile, true, false));
    }


    public void testSimpleTest1() {
        doTest(true);
    }

    public void testComplexScript() {
        doTest(true);
    }

    public void testEmptyExprList() {
        doTest(true);
    }

    public void testFunctionDefinition() {
        doTest(true);
    }

    public void testInvalidSymbol() {
        doTest(true);
    }

    public void testInvalidAssignment() {
        doTest(true);
    }


    ////
    //// Test framework setup functions
    ////

    public RParserTest() {
        super("parser", "R", new RParserDefinition());
    }

    @Override
    protected String getTestDataPath() {
        return "misc/testData";
    }

    @Override
    protected boolean skipSpaces() {
        return true;
    }
}
