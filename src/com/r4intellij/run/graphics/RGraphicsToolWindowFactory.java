package com.r4intellij.run.graphics;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentManager;
import org.jetbrains.annotations.NotNull;

public class RGraphicsToolWindowFactory implements ToolWindowFactory {

    @Override
    public void createToolWindowContent(@NotNull final Project project, @NotNull final ToolWindow toolWindow) {
        final ContentManager contentManager = toolWindow.getContentManager();

        final Content content = contentManager.getFactory().createContent(
                new RGraphicsToolWindow(RGraphicsUtils.getGraphicsState(project)),
                null,
                false
        );

        contentManager.addContent(content);
    }
}
