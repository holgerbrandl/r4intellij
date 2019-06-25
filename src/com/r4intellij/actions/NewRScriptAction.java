/*
 * Copyright (c) Joachim Ansorg, mail@ansorg-it.com
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.r4intellij.actions;

import com.intellij.CommonBundle;
import com.intellij.ide.actions.CreateElementActionBase;
import com.intellij.ide.fileTemplates.FileTemplate;
import com.intellij.ide.fileTemplates.FileTemplateManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.RFileType;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.util.Properties;

import static com.r4intellij.RFileType.DOT_R_EXTENSION;


/**
 * Action to create a new R script from a template.
 * <br>
 * The template data is stored in resources/fileTemplates/internal/template_script.R.ft
 *
 * @author jansorg
 * @author holgerbrandl
 */
public class NewRScriptAction extends CreateElementActionBase {


    static final String DEFAULT_TEMPLATE_FILENAME = "R Script.R";


    public NewRScriptAction() {
        super("R Script", "Create a new R script", RFileType.INSTANCE.getIcon());
    }


    static String computeFilename(String inputFilename) {
        String usedExtension = FileUtilRt.getExtension(inputFilename);
        boolean withExtension = !usedExtension.isEmpty();

        return withExtension ? inputFilename : (inputFilename + DOT_R_EXTENSION);
    }


    private String getDialogPrompt() {
        return "Creates a new R script";
    }


    private String getDialogTitle() {
        return "Create R Script";
    }


    protected String getCommandName() {
        return "R Script";
    }


    protected String getActionName(PsiDirectory directory, String newName) {
        return getDialogTitle();
    }


    @NotNull
    protected final PsiElement[] invokeDialog(final Project project, final PsiDirectory directory) {
        final MyInputValidator validator = new MyInputValidator(project, directory);
        Messages.showInputDialog(project, getDialogPrompt(), getDialogTitle(), RFileType.INSTANCE.getIcon(), "", validator);

        return validator.getCreatedElements();
    }


    @NotNull
    protected PsiElement[] create(String newName, PsiDirectory directory) throws Exception {
        PsiFile file = createFromTemplate(directory, computeFilename(newName), DEFAULT_TEMPLATE_FILENAME);

        PsiElement child = file.getLastChild();
        return child != null ? new PsiElement[]{file, child} : new PsiElement[]{file};
    }


    protected String getErrorTitle() {
        return CommonBundle.getErrorTitle();
    }


    @NotNull
    static PsiFile createFromTemplate(final PsiDirectory directory, String fileName, String templateName) throws IncorrectOperationException {
        Project project = directory.getProject();
        FileTemplateManager templateManager = FileTemplateManager.getInstance(project);
        FileTemplate template = templateManager.getInternalTemplate(templateName);

        Properties properties = new Properties(templateManager.getDefaultProperties());

        String templateText;
        try {
            templateText = template.getText(properties);
        } catch (IOException e) {
            String templateToSubject = templateManager.internalTemplateToSubject(templateName);
            throw new RuntimeException("Unable to load template for " + templateToSubject, e);
        }

        PsiFile file = PsiFileFactory.getInstance(project)
                .createFileFromText(fileName, RFileType.INSTANCE, templateText);

        return (PsiFile) directory.add(file);
    }
}
