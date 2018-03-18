package com.r4intellij.editor;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementPresentation;
import com.intellij.openapi.util.IconLoader;
import com.intellij.util.PlatformIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.Icon;
import java.io.File;

public class PathLookupElement extends LookupElement {
    private static final Icon textIcon = IconLoader.getIcon("/fileTypes/text.png");
    private final String path;
    private final String file;
    private final Boolean isDirectory;

    PathLookupElement(String path, Boolean isDirectory) {
        this.path = path;
        this.isDirectory = isDirectory;

        int pos = path.lastIndexOf('/');
        file = pos < 0 ? path : path.substring(pos + 1);
    }

    @Override
    public void renderElement(LookupElementPresentation presentation) {
        presentation.setItemText(file);
        presentation.setIcon(isDirectory ? PlatformIcons.DIRECTORY_CLOSED_ICON : textIcon);
    }

    @NotNull
    @Override
    public String getLookupString() {
        return path;
    }

    @Override
    public boolean isCaseSensitive() {
        return true;
    }
}
