package com.r4intellij.packages.remote.ui;


import com.google.common.collect.Lists;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.InputValidator;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.AnActionButton;
import com.intellij.ui.AnActionButtonRunnable;
import com.intellij.ui.CheckBoxList;
import com.intellij.ui.ToolbarDecorator;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.packages.remote.RDefaultRepository;
import com.r4intellij.packages.remote.RPackageManagementService;
import com.r4intellij.packages.remote.RRepository;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.List;

public class RManageRepoDialog extends DialogWrapper {
    private JPanel myMainPanel;
    private CheckBoxList<RRepository> myList;
    private int currentCRANMirror;
    private RPackageManagementService myController;


    public RManageRepoDialog(@Nullable final Project project, @NotNull final RPackageManagementService controller) {
        super(project, false);

        setTitle("Manage Repositories");
        myMainPanel = new JPanel();
        myList = new CheckBoxList();
        final JPanel repositoryList = createRepositoriesList();
        myMainPanel.add(repositoryList);

        myController = controller;
        reloadList();

        init();
    }


    private void reloadList() {
        myList.clear();
        final List<RDefaultRepository> repositories = myController.getDefaultRepositories();
        RPackageService service = RPackageService.getInstance();

        for (RDefaultRepository repository : repositories) {
            myList.addItem(repository, repository.getUrl(), service.enabledRepositories.contains(repository.getUrl()));
        }

        for (String repository : service.userRepositories) {
            myList.addItem(new RRepository(repository), repository, true);
        }
    }


    private JPanel createRepositoriesList() {
        return ToolbarDecorator.createDecorator(myList)
                .disableUpDownActions()
                .setAddAction(new AnActionButtonRunnable() {
                    @Override
                    public void run(AnActionButton button) {
                        String url = Messages.showInputDialog("Please input repository URL", "Repository URL", null);
                        myList.addItem(new RRepository(url), url, true);
                    }
                })
                .setEditAction(new AnActionButtonRunnable() {
                    @Override
                    public void run(AnActionButton button) {
                        final int index = myList.getSelectedIndex();
                        final RRepository oldValue = (RRepository) myList.getItemAt(index);
                        if (oldValue != null && oldValue.getUrl().equals("@CRAN@")) {
                            List<String> mirrorsList = myController.getMirrors();
                            String[] mirrors = mirrorsList.toArray(new String[mirrorsList.size()]);
                            currentCRANMirror = Messages.showChooseDialog("", "Choose CRAN mirror", mirrors,
                                    mirrors[myController.getCRANMirror()], null);
                        } else {
                            String url =
                                    Messages.showInputDialog("Please edit repository URL", "Repository URL", null, oldValue.getUrl(), new InputValidator() {
                                        @Override
                                        public boolean checkInput(String inputString) {
                                            return !StringUtil.isEmptyOrSpaces(inputString);
                                        }


                                        @Override
                                        public boolean canClose(String inputString) {
                                            return true;
                                        }
                                    });
                            if (!StringUtil.isEmptyOrSpaces(url) && !oldValue.getUrl().equals(url)) {
                                myList.updateItem(oldValue, new RRepository(url), url);
                            }
                        }
                    }
                })
                .setRemoveAction(button -> {
                    RPackageService service = RPackageService.getInstance();
                    final int index = myList.getSelectedIndex();
                    final RRepository selected = myList.getItemAt(index);
                    if (selected != null && service.userRepositories.contains(selected.getUrl())) {
                        service.userRepositories.remove(selected.getUrl());
                    }
                    reloadList();
                })
                .setRemoveActionUpdater(event -> {
                    final int index = myList.getSelectedIndex();
                    return !(myList.getItemAt(index) instanceof RDefaultRepository);
                })
                .createPanel();
    }


    @Override
    protected void doOKAction() {
        this.processDoNotAskOnOk(0);
        if (this.getOKAction().isEnabled()) {
            List<RRepository> enabled = Lists.newArrayList();
            for (int i = 0; i < myList.getItemsCount(); i++) {
                if (myList.isItemSelected(i)) {
                    final Object item = myList.getItemAt(i);
                    enabled.add((RRepository) item);
                }
            }
            myController.setCRANMirror(currentCRANMirror);
            myController.setRepositories(enabled);
            this.close(0);
        }
    }


    @Nullable
    @Override
    protected JComponent createCenterPanel() {
        return myMainPanel;
    }
}
