

package com.r4intellij.actions;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionManager;
import com.intellij.execution.console.ProcessBackedConsoleExecuteActionHandler;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.lang.injection.InjectedLanguageManager;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Caret;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogBuilder;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RFileType;
import com.r4intellij.console.RConsoleRunner;
import com.r4intellij.psi.api.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Objects;


/**
 * Event handler for the "Run Selection" action within an Arc code editor - runs the currently selected text within the
 * current REPL.
 */
public class EvalSelectionOrExprAction extends AnAction {

    public void actionPerformed(AnActionEvent e) {
        Editor editor = e.getData(PlatformDataKeys.EDITOR);
        if (editor == null) {
            return;
        }

        // https://intellij-support.jetbrains.com/hc/en-us/community/posts/207379815-AnActionEvent-cannot-share-data-context-between-Swing-events-
        final Project project = editor.getProject();
        if (project == null) {
            throw new RuntimeException("no project in " + e);
        }


        String exprOrSelect = editor.getSelectionModel().getSelectedText();


        // double check if user has selected complete document
        if (exprOrSelect != null && exprOrSelect.length() == e.getData(PlatformDataKeys.PSI_FILE).getTextLength()) {
            DialogBuilder db = new DialogBuilder();

            db.setTitle("Execute all?");
            db.setCenterPanel(new JLabel("Do you want to evaluate the complete file in the console?"));
            db.addOkAction();
            db.addCancelAction();

            if (!db.showAndGet()) return;
        }

        // https://intellij-support.jetbrains.com/hc/en-us/community/posts/206150409-How-do-I-get-to-PSI-from-a-document-or-editor-?page=1#community_comment_115000085244


        // if nothing is selected, detect expression under caret
        if (StringUtil.isEmptyOrSpaces(exprOrSelect)) {
            // old approach; maybe restore this feature via config entry or different keybinding
//            editor.getSelectionModel().selectLineAtCaret();

            // PsiManager psiManager = PsiManager.getInstance(project);
            // final PsiFile psiFile = psiManager.findFile(null);
            PsiFile psiFile = e.getData(PlatformDataKeys.PSI_FILE);

            if (psiFile != null) {
                PsiElement element = getCaretElement(editor, psiFile);

                // resolve injected chunk elements if file-type is not R
                if (!element.getContainingFile().getFileType().equals(RFileType.INSTANCE)) {
                    // from https://intellij-support.jetbrains.com/hc/en-us/community/posts/206116909-Access-editor-within-PsiReferenceContributor-Originally-injected-PsiElement
                    element = InjectedLanguageManager.getInstance(project).findInjectedElementAt(psiFile, editor.getCaretModel().getOffset());
                }

                // grow until we reach expression barrier
                PsiElement evalElement = PsiTreeUtil.findFirstParent(element, psiElement -> {
                    // avoid that { would be evaluated if cursor is set after block
                    if (psiElement instanceof LeafPsiElement) return false;

                    PsiElement parent = psiElement.getParent();
                    return parent instanceof RBlockExpression ||
                            parent instanceof RWhileStatement ||
                            parent instanceof RForStatement ||
                            parent instanceof RIfStatement ||
                            parent instanceof RFile;
                });

                if (evalElement == null) return;

                exprOrSelect = evalElement.getText();

                // set caret to next downstream element
                // todo add preference for caret transition after eval
                PsiElement nextSibling = PsiTreeUtil.getNextSiblingOfType(evalElement, RExpression.class);
                if (nextSibling != null) {
                    int siblingEndPos = nextSibling.getTextOffset() + nextSibling.getTextLength();
                    editor.getCaretModel().getCurrentCaret().moveToOffset(siblingEndPos);
                }
            }
        }


        // just proceed if we have any code to be evaluated
        if (StringUtil.isEmptyOrSpaces(exprOrSelect)) return;


        // either use existing runner or create new console if none is open yet
        ExecutionManager executionManager = ExecutionManager.getInstance(project);
        RunContentDescriptor descriptor = executionManager.getContentManager().getSelectedContent();


        // if current descriptor is not r-console fall back to the others and use last
        if (descriptor == null) {
            descriptor = executionManager.getContentManager().getAllDescriptors().stream().
                    filter(d -> Objects.equals(d.getDisplayName(), "R Console")).
                    reduce((first, second) -> second).orElse(null); // get last descriptor

        }


        // create a console if there's none yet
        if (descriptor == null) {
            try {
                RConsoleRunner runner = new RConsoleRunner(project, project.getBasePath());
                runner.initAndRun();

                descriptor = executionManager.getContentManager().getSelectedContent();
            } catch (ExecutionException e1) {
                e1.printStackTrace();
            }
        }


        // finally run the code
        if (descriptor != null && descriptor.getProcessHandler() != null) {
            ProcessBackedConsoleExecuteActionHandler execHandler =
                    new ProcessBackedConsoleExecuteActionHandler(descriptor.getProcessHandler(), false);

            execHandler.processLine(exprOrSelect.trim());
        }
    }


    @Nullable
    private static PsiElement getCaretElement(Editor editor, PsiFile psiFile) {

        // why not PsiUtilBase.getElementAtCaret(editor) ??
        // todo rather fix and use https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000126084-Incorrect-help-when-caret-at-word-end

        // is caret at line start
        CaretModel caretModel = editor.getCaretModel();
        Caret curCaret = caretModel.getCurrentCaret();

        // return nothing if caret is in empty line
        // FIXME lines that just contain whitespace are not ignored
        if (curCaret.getVisualLineEnd() - curCaret.getVisualLineStart() == 0) return null;

        // TODO isn't there a an existing utility method for this? IJ does for most editor actions
//        TargetElementUtil.findTargetElement(editor, curCaret.getOffset())
        PsiElement element = psiFile.findElementAt(caretModel.getOffset());

        if (element == null && caretModel.getOffset() > 0) {
            element = psiFile.findElementAt(caretModel.getOffset() - 1);
        }

        if (element == null) return null;


        if (curCaret.getVisualLineStart() == curCaret.getOffset()) {
            return PsiTreeUtil.nextVisibleLeaf(element);
        }

        if (curCaret.getVisualLineEnd() - 1 == curCaret.getOffset()) {
            return PsiTreeUtil.prevVisibleLeaf(element);
        }

        return element;
    }


    @Override
    public void update(@NotNull final AnActionEvent e) {
        final Project project = CommonDataKeys.PROJECT.getData(e.getDataContext());
        e.getPresentation().setVisible(project != null);
    }
}
