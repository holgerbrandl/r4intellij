package com.r4intellij.run.debug.resolve;

import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.PsiElementProcessor;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RFunctionExpression;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.r4intellij.debugger.data.RFunctionConstants.MAIN_FUNCTION_NAME;

class RFunctionDefinitionProcessor implements PsiElementProcessor<PsiElement> {

    @NotNull
    private final Document myDocument;

    @NotNull
    private final RFunctionDescriptor myRoot;


    public RFunctionDefinitionProcessor(@NotNull final Document document) {
        myDocument = document;

        myRoot = new RFunctionDescriptor(
                MAIN_FUNCTION_NAME,
                null,
                0,
                Integer.MAX_VALUE
        );
    }


    @Override
    public boolean execute(@NotNull final PsiElement element) {
        if (element instanceof RFunctionExpression && element.getParent() instanceof RAssignmentStatement) {
            final RAssignmentStatement parent = (RAssignmentStatement) element.getParent();
            final int startOffset = parent.getTextOffset();

            // `RAssignmentStatement` couldn't be without name
            assert parent.getName() != null;

            add(
                    myRoot,
                    parent.getName(),
                    myDocument.getLineNumber(startOffset),
                    myDocument.getLineNumber(startOffset + parent.getTextLength())
            );
        }

        return true;
    }


    @NotNull
    public RFunctionDescriptor getRoot() {
        return myRoot;
    }


    private void add(@NotNull final RFunctionDescriptor currentDescriptor,
                     @NotNull final String name,
                     final int startLine,
                     final int endLine) {
        if (!trySiftDown(currentDescriptor, name, startLine, endLine)) {
            addAsChild(currentDescriptor, name, startLine, endLine);
        }
    }


    private boolean trySiftDown(@NotNull final RFunctionDescriptor currentDescriptor,
                                @NotNull final String name,
                                final int startLine,
                                final int endLine) {
        for (final List<RFunctionDescriptor> sameNameChildren : currentDescriptor.getChildren().values()) {
            for (final RFunctionDescriptor child : sameNameChildren) {
                if (child.getStartLine() <= startLine && endLine <= child.getEndLine()) {
                    add(
                            child,
                            name,
                            startLine,
                            endLine
                    );

                    return true;
                }
            }
        }

        return false;
    }


    private void addAsChild(@NotNull final RFunctionDescriptor currentDescriptor,
                            @NotNull final String name,
                            final int startLine,
                            final int endLine) {
        final Map<String, List<RFunctionDescriptor>> children = currentDescriptor.getChildren();

        if (!children.containsKey(name)) {
            children.put(
                    name,
                    new ArrayList<RFunctionDescriptor>()
            );
        }

        children.get(name).add(
                new RFunctionDescriptor(
                        name,
                        currentDescriptor,
                        startLine,
                        endLine
                )
        );
    }
}
