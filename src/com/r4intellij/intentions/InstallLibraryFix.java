package com.r4intellij.intentions;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionManager;
import com.intellij.execution.Executor;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.ui.RunContentDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.r4intellij.console.RConsoleRunner;
import com.r4intellij.packages.RSkeletonGenerator;
import org.jetbrains.annotations.NotNull;


/**
 * Also see http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
 *
 * @author Holger Brandl
 */
public class InstallLibraryFix implements LocalQuickFix {

    final private String packageName;


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

//            packageName = "docopt";

//            runner.getConsoleExecuteActionHandler().processLine("options(install.packages.check.source = \"no\")");
            runner.getConsoleExecuteActionHandler().processLine("install.packages('" + packageName + "')");
//            runner.getConsoleExecuteActionHandler().processLine("install.packages('" + packageName + "',  type=\"source\")");

            // test installation was successful
//            runner.getConsoleExecuteActionHandler().processLine("require(" + packageName + ")");
            runner.getConsoleExecuteActionHandler().processLine(
                    "cat(\"" + packageName + " installed:\", \"" + packageName + "\" %in% rownames(installed.packages()), '\\n')"
            );

            // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000038590-How-to-close-Run-console-tab-window-programmatically-
            runner.getProcessHandler().addProcessListener(new ProcessAdapter() {

                @Override
                public void onTextAvailable(ProcessEvent event, Key outputType) {
                    String expected = packageName + " installed: TRUE";

                    if (event.getText().trim().equals(expected)) {
                        // installation looks good, so lets close the window
                        runner.getConsoleExecuteActionHandler().processLine("quit(\"no\")");

                        // update package index
                        RSkeletonGenerator.updateSkeletons(project);
                    }
                }


                @Override
                public void processTerminated(ProcessEvent event) {
                    ExecutionManager executionManager = ExecutionManager.getInstance(project);
                    RunContentDescriptor descriptor = executionManager.getContentManager().getSelectedContent();

                    // don't do nothing if user has terminated console window
                    if (descriptor == null) {
                        return;
                    }
//                    Set<Executor> executors = ((ExecutionManagerImpl) executionManager).getExecutors(descriptor);

                    ApplicationManager.getApplication().invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            ApplicationManager.getApplication().runWriteAction(new Runnable() {
                                @Override
                                public void run() {
                                    Executor exec = DefaultRunExecutor.getRunExecutorInstance();
                                    executionManager.getContentManager().removeRunContent(exec, descriptor);
                                }
                            });
                        }
                    });
                }


            });

        } catch (ExecutionException e) {
            e.printStackTrace();
        }


    }
}
