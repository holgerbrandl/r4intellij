package com.r4intellij;

import com.intellij.ide.FileIconProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.util.Iconable;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;


// see http://stackoverflow.com/questions/29298625/change-icon-of-file-in-project-view-tool-window
// not used for now because RFileType.getIcon should be enough
public class RFileIconProvider implements FileIconProvider {
    @Nullable
    @Override
    public Icon getIcon(@NotNull VirtualFile virtualFile, @Iconable.IconFlags int i, @Nullable Project project) {
        if (virtualFile.getFileType().equals(RFileType.INSTANCE)) {
            return IconLoader.findIcon("/icons/r_logo_16.png");
//            return AllIcons.Icons.LANGUAGE_ICON;
        }
        return null;
    }
}
