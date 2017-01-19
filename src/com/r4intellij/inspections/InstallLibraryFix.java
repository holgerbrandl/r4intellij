/*
 * Copyright 2011-2011 Gregory Shrago
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.r4intellij.console.RConsoleRunner;
import com.r4intellij.packages.RPackageService;
import org.jetbrains.annotations.NotNull;


/**
 * Also see http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
 * @author Holger Brandl
 *
 */
public class InstallLibraryFix implements LocalQuickFix {

    private final String packageName;


    public InstallLibraryFix(String name) {
        packageName = name;
    }


    @NotNull
    @Override
    public String getName() {
        return "Install '" + packageName + "'";
    }


    @NotNull
    @Override
    public String getFamilyName() {
        return "Dependency management";
    }


    @Override
    public void applyFix(final @NotNull Project project, @NotNull ProblemDescriptor descriptor) {
        RConsoleRunner runner = new RConsoleRunner(project, project.getBasePath());

        try {
            runner.initAndRun();
            runner.getConsoleExecuteActionHandler().processLine("chooseCRANmirror(ind = 1)");

            // TODO detect repository here to do biocite instead of install.packages if needed
            // see com.r4intellij.packages.remote.RepoUtils.loadAvailablePackages()

            // see ?install.packages
//            runner.getConsoleExecuteActionHandler().processLine("options(install.packages.check.source = \"no\")");
//            runner.getConsoleExecuteActionHandler().processLine("install.packages('" + packageName + "')");
            runner.getConsoleExecuteActionHandler().processLine("install.packages('" + packageName + "',  type=\"source\")");

            // test installation was successful
            runner.getConsoleExecuteActionHandler().processLine("require(" + packageName + ")");

            // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/206148229-Force-inspection-rerun-
            runner.getProcessHandler().addProcessListener(new ProcessAdapter() {
                @Override
                public void onTextAvailable(ProcessEvent event, Key outputType) {
                    String expected = "Loading required package: " + packageName;
                    String expectedFailed = "there is no package called ‘" + packageName + "’";

                    if (event.getText().trim().equals(expected)) {
                        // update package index
                        RPackageService.getInstance().refreshIndex();

                        // todo quit console session and close the tool winodw tab
//                        runner.getConsoleExecuteActionHandler().processLine("quit(\"no\")");
//                        runner.getConsoleView().
                    }
                }
            });

        } catch (ExecutionException e) {
            e.printStackTrace();
        }


    }
}
