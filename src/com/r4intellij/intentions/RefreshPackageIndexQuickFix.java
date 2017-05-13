package com.r4intellij.intentions;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.ActionPlaces;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.project.Project;
import com.r4intellij.actions.RefreshSkeletonsAction;
import org.jetbrains.annotations.NotNull;


/**
 * @author Holger Brandl
 */
public class RefreshPackageIndexQuickFix implements LocalQuickFix {


    @NotNull
    @Override
    public String getName() {
        //noinspection DialogTitleCapitalization
        return "Refresh R Package Index";
    }


    @NotNull
    @Override
    public String getFamilyName() {
        return "NA";
    }


    @Override
    public void applyFix(final @NotNull Project project, @NotNull ProblemDescriptor descriptor) {
        RefreshSkeletonsAction refreshAction = (RefreshSkeletonsAction)
                ActionManager.getInstance().getAction(RefreshSkeletonsAction.class.getCanonicalName());
//        DataManager.getInstance().getDataContext()
//        DataContext dataContext = DataManager.getInstance().getDataContext(new JPanel());
        DataContext dataContext = DataManager.getInstance().getDataContext();

        refreshAction.actionPerformed(new AnActionEvent(null, dataContext, ActionPlaces.UNKNOWN,
                refreshAction.getTemplatePresentation(), ActionManager.getInstance(), 0));
    }
}
