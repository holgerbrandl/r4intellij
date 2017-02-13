package com.r4intellij.settings;

import com.google.common.collect.Lists;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.r4intellij.interpreter.RSkeletonGenerator;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.ArrayList;

public class RSettingsConfigurable implements SearchableConfigurable, Configurable.NoScroll {
    private final Project myProject;

    private JPanel settingsPanel;

    private TextFieldWithBrowseButton interpreterPathField;


    RSettingsConfigurable(Project project) {
        myProject = project;
    }


    @NotNull
    @Override
    public String getId() {
        return getClass().getName();
    }


    @Nullable
    @Override
    public Runnable enableSearch(String option) {
        return null;
    }


    @Nls
    @Override
    public String getDisplayName() {
        return "R Interpreter";
    }


    @Nullable
    @Override
    public String getHelpTopic() {
        return null;
    }


    @Nullable
    @Override
    public JComponent createComponent() {
//        interpreterPathField.setText(get);
        return settingsPanel;
    }


    @Override
    public boolean isModified() {
        final RSettings interpreterService = RSettings.getInstance();
        return !interpreterService.getInterpreterPath().equals(interpreterPathField.getText());// ||
//                !interpreterService.getSourcesPath().equals(mySourcesField.getText());
    }


    @Override
    public void apply() throws ConfigurationException {
        final RSettings interpreterService = RSettings.getInstance();
        final String interpreterPath = interpreterPathField.getText();


//        final String oldSourcesPath = interpreterService.getSourcesPath();
//
//            if (!StringUtil.isEmptyOrSpaces(oldSourcesPath)) {
//                detachLibrary(myProject, R_LIBRARY);
//            }
//            if (!StringUtil.isEmptyOrSpaces(sourcesPath)) {
//                final ArrayList<String> paths = getSourcePaths(sourcesPath);
//                if (!paths.isEmpty()) {
//                    createLibrary(R_LIBRARY, paths, myProject);
//                }
//            }
//        }

        // also create additional library
        LibraryUtil.createLibrary(LibraryUtil.R_LIBRARY, Lists.newArrayList(), myProject);


        RSkeletonGenerator.generateSkeletons(myProject);
//        interpreterService.setSourcesPath(sourcesPath);
        interpreterService.setInterpreterPath(interpreterPath);
    }


    private ArrayList<String> getSourcePaths(@NotNull final String sourcesPath) {
        final ArrayList<String> paths = Lists.newArrayList();
        final VirtualFile file = LocalFileSystem.getInstance().findFileByPath(sourcesPath);
        if (file != null) {
            final VirtualFile libFile = file.findFileByRelativePath("src/library");
            if (libFile != null) {
                final VirtualFile[] children = libFile.getChildren();
                for (VirtualFile child : children) {
                    final VirtualFile rDirectory = child.findFileByRelativePath("R");
                    if (rDirectory != null) {
                        paths.add(rDirectory.getPath());
                    }
                }
            }
        }
        return paths;
    }


    @Override
    public void reset() {
        final RSettings interpreterService = RSettings.getInstance();

        final String interpreterPath = interpreterService.getInterpreterPath();
        interpreterPathField.setText(interpreterPath != null ? interpreterPath : "");
    }


    @Override
    public void disposeUIResources() {
    }


    private void createUIComponents() {
        interpreterPathField = new TextFieldWithBrowseButton();

        final FileChooserDescriptor interpreterDescriptor = FileChooserDescriptorFactory.createSingleLocalFileDescriptor();
        interpreterPathField.addBrowseFolderListener("Choose interpreter path", "Choose interpreter path", myProject, interpreterDescriptor);
    }
}
