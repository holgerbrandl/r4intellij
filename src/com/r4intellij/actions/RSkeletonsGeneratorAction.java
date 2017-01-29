package com.r4intellij.actions;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.Presentation;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.r4intellij.interpreter.RSkeletonGenerator;


public class RSkeletonsGeneratorAction extends AnAction {

    private static final Logger LOG = Logger.getInstance(RSkeletonsGeneratorAction.class);


    public void actionPerformed(AnActionEvent event) {
        final Project project = event.getProject();
        assert project != null;

        RSkeletonGenerator.generateSkeletons(project);
    }


    @Override
    public void update(AnActionEvent e) {
        Presentation presentation = e.getPresentation();
//        if (!ApplicationManager.getApplication().isInternal()) {
//            presentation.setEnabled(false);
//            presentation.setVisible(false);
//            return;
//        }
        presentation.setVisible(true);
        presentation.setEnabled(true);
    }


}
