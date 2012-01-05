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
import com.r4intellij.lang.parser.RParserDefinition;
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
//        ASTNode astNode = parseThis("library(stringr);");
        ASTNode astNode = parseThis("dfd@sdsd =34;");
        System.out.println(Arrays.toString(astNode.getChildren(new TokenSet())));
        System.out.println(astNode.getText());
        System.out.println(astNode.toString());
        System.out.println(astNode.getPsi());
    }

}
