package com.r4intellij.run.debug.resolve;

import com.intellij.openapi.editor.Document;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.PlatformTestCase;
import com.r4intellij.debugger.data.RFunctionConstants;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class RFunctionDefinitionProcessorTest extends PlatformTestCase {

    public void testNotFunction() throws IOException {
        final String text = "x <- c(1:3)";
        final RFunctionDescriptor root = calculateRootDescriptor(text);

        assertTrue(root.getChildren().isEmpty());
    }


    public void testFunction() throws IOException {
        final String text = "f <- function() {\nprint(\"ok\")\n}";
        final RFunctionDescriptor root = calculateRootDescriptor(text);
        final Map<String, List<RFunctionDescriptor>> rootChildren = root.getChildren();

        assertEquals(1, rootChildren.size());
        assertEquals(1, rootChildren.get("f").size());

        final RFunctionDescriptor f = rootChildren.get("f").get(0);

        assertEquals("f", f.getName());
        assertEquals(root, f.getParent());
        assertEquals(0, f.getStartLine());
        assertEquals(2, f.getEndLine());
        assertTrue(f.getChildren().isEmpty());
    }


    public void testInnerFunction() throws IOException {
        final String text = "f <- function() {\nd <- function(x) {\nx\n}\nprint(\"ok\")\n}";
        final RFunctionDescriptor root = calculateRootDescriptor(text);
        final Map<String, List<RFunctionDescriptor>> rootChildren = root.getChildren();

        assertEquals(1, rootChildren.size());
        assertEquals(1, rootChildren.get("f").size());

        final RFunctionDescriptor f = rootChildren.get("f").get(0);

        assertEquals("f", f.getName());
        assertEquals(root, f.getParent());
        assertEquals(0, f.getStartLine());
        assertEquals(5, f.getEndLine());
        assertEquals(1, f.getChildren().size());
        assertEquals(1, f.getChildren().get("d").size());

        final RFunctionDescriptor d = f.getChildren().get("d").get(0);

        assertEquals("d", d.getName());
        assertEquals(f, d.getParent());
        assertEquals(1, d.getStartLine());
        assertEquals(3, d.getEndLine());
        assertTrue(d.getChildren().isEmpty());
    }


    public void testInnerInnerFunction() throws IOException {
        final String text = "f <- function() {\nd <- function(x) {\ng <- function(x) {\nx + 1\n}\nx\n}\nprint(\"ok\")\n}";
        final RFunctionDescriptor root = calculateRootDescriptor(text);
        final Map<String, List<RFunctionDescriptor>> rootChildren = root.getChildren();

        assertEquals(1, rootChildren.size());
        assertEquals(1, rootChildren.get("f").size());

        final RFunctionDescriptor f = rootChildren.get("f").get(0);

        assertEquals("f", f.getName());
        assertEquals(root, f.getParent());
        assertEquals(0, f.getStartLine());
        assertEquals(8, f.getEndLine());
        assertEquals(1, f.getChildren().size());
        assertEquals(1, f.getChildren().get("d").size());

        final RFunctionDescriptor d = f.getChildren().get("d").get(0);

        assertEquals("d", d.getName());
        assertEquals(f, d.getParent());
        assertEquals(1, d.getStartLine());
        assertEquals(6, d.getEndLine());
        assertEquals(1, d.getChildren().size());
        assertEquals(1, d.getChildren().get("g").size());

        final RFunctionDescriptor g = d.getChildren().get("g").get(0);

        assertEquals("g", g.getName());
        assertEquals(d, g.getParent());
        assertEquals(2, g.getStartLine());
        assertEquals(4, g.getEndLine());
        assertTrue(g.getChildren().isEmpty());
    }


    public void testOverriddenFunction() throws IOException {
        final String text = "f <- function() {\nprint(\"ok\")\n}\n\nf <- function() {\nprint(\"ok\")\n}";
        final RFunctionDescriptor root = calculateRootDescriptor(text);
        final Map<String, List<RFunctionDescriptor>> rootChildren = root.getChildren();

        assertEquals(1, rootChildren.size());
        assertEquals(2, rootChildren.get("f").size());

        final RFunctionDescriptor f1 = rootChildren.get("f").get(0);

        assertEquals("f", f1.getName());
        assertEquals(root, f1.getParent());
        assertEquals(0, f1.getStartLine());
        assertEquals(2, f1.getEndLine());
        assertTrue(f1.getChildren().isEmpty());

        final RFunctionDescriptor f2 = rootChildren.get("f").get(1);

        assertEquals("f", f2.getName());
        assertEquals(root, f2.getParent());
        assertEquals(4, f2.getStartLine());
        assertEquals(6, f2.getEndLine());
        assertTrue(f2.getChildren().isEmpty());
    }


    @NotNull
    private RFunctionDescriptor calculateRootDescriptor(@NotNull final String text) throws IOException {
        final VirtualFile virtualFile = getVirtualFile(createTempFile("script.r", text));
        assert virtualFile != null;

        final PsiFile psiFile = PsiManager.getInstance(getProject()).findFile(virtualFile);
        assert psiFile != null;

        final Document document = PsiDocumentManager.getInstance(getProject()).getDocument(psiFile);
        assert document != null;

        final RFunctionDefinitionProcessor processor = new RFunctionDefinitionProcessor(document);

        PsiTreeUtil.processElements(processor, psiFile);

        final RFunctionDescriptor root = processor.getRoot();

        assertEquals(RFunctionConstants.MAIN_FUNCTION_NAME, root.getName());
        assertEquals(null, root.getParent());
        assertEquals(0, root.getStartLine());
        assertEquals(Integer.MAX_VALUE, root.getEndLine());

        return root;
    }
}