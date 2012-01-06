/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package de.intellij4r.lang;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.PsiParser;
import com.intellij.lang.impl.PsiBuilderImpl;
import com.intellij.openapi.command.impl.DummyProject;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.testFramework.IdeaTestCase;
import com.intellij.testFramework.LightPlatformTestCase;
import com.r4intellij.Utils;
import com.r4intellij.lang.parser.RParserDefinition;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;


/**
 * Test the parser against some example R-scriptlets.
 *
 * @author Holger Brandl
 */
public class RParserTest extends LightPlatformTestCase {

    private static final IFileElementType ROOT = new IFileElementType("ROOT", Language.ANY);


    public RParserTest() {
        IdeaTestCase.initPlatformPrefix();
    }

    private ASTNode parseThis(CharSequence contents) {
        RParserDefinition parserDefinition = new RParserDefinition();
        Project project = DummyProject.getInstance();
        PsiParser parser = parserDefinition.createParser(project);

        PsiBuilderImpl builder = new PsiBuilderImpl(project, null, parserDefinition, parserDefinition.createLexer(project), null, contents, null, null);
        return parser.parse(ROOT, builder);
//        return parser.parse(parserDefinition.getFileNodeType(), builder);
    }

    @Test
    public void testImport() {
        ASTNode astNode = parseThis("\nlibrary(stringr);");
        System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));
        System.out.println(astNode.getText());
        System.out.println(astNode.toString());
        System.out.println(astNode.getPsi());

    }

    @Test
    public void testSnippet() {
        ASTNode astNode = parseThis("\n" +
                "# use another normality test\n" +
                "library(nortest)\n" +
                "\n" +
                "\n" +
                "somedata <- R$\"counts\";\n" +
                "\n" +
                "hist(somedata)\n" +
                "shapiro.test(somedata);");
        System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));
        System.out.println(astNode.getText());
        System.out.println(astNode.toString());
        System.out.println(astNode.getPsi());
    }

    @Test
    public void testSlotAssignment() {
        ASTNode astNode = parseThis("dfd@sdsd =34;");
        System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));
        System.out.println(astNode.getText());
        System.out.println(astNode.toString());
        System.out.println(astNode.getPsi());
    }

    @Test
    public void testSlotAcess() {
        ASTNode astNode = parseThis("somedata <- R$\"Nuclei DAPI - Number of Objects.boxcox\";");
        System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));
//        System.out.println(astNode.);
        System.out.println(astNode.getText());
        System.out.println(astNode.toString());
        System.out.println(astNode.getPsi());
    }

    @Test
    public void testInvalidSymbolName() {
        ASTNode astNode = null;
        try {
            astNode = parseThis("22aa <-1");
            System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));

            Assert.fail();
        } catch (Exception e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
    }

    @Test
    public void testNestedFunctionCall() {
        ASTNode astNode = null;
        astNode = parseThis("foo(bar(23));");
        System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));
    }

    @Test
    public void testIdFunDef() {
        ASTNode astNode = null;
        astNode = parseThis("function(x,...) { x };");
        System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));
    }


    @Test
    public void testComplexTokenization() {
        String testData = Utils.readFileAsString("misc/complex_script.R");
//        String testData = Utils.readFileAsString("misc/normality tests.R");
        ASTNode astNode = parseThis(testData);
        System.out.println(astNode);
    }
}
