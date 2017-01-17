package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.spellchecker.SpellCheckerManager;
import com.intellij.spellchecker.dictionary.EditableDictionary;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * @author avesloguzova
 * @author holgerbrandl
 */
@State(name = "RPackageService",
        storages = {
                @Storage(file = "rpackages.xml") // i.e. ~/Library//Caches/IntelliJIdea2016.1/plugins-sandbox/config/options/rpackages.xml
        }
)
public class RPackageService implements PersistentStateComponent<RPackageService> {

    public Set<RPackage> allPackages = ContainerUtil.newConcurrentSet();
//    public Set<RPackage> packages = ContainerUtil.newConcurrentSet();

    public int CRANMirror = 1;

    public List<String> enabledRepositories = Lists.newArrayList();

    public List<String> userRepositories = Lists.newArrayList();


    @Nullable
    @Override
    public RPackageService getState() {
        return this;
    }


    @Override
    public void loadState(RPackageService state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    public static RPackageService getInstance() {
        return ServiceManager.getService(RPackageService.class);
    }


    public Set<RPackage> getPackages() {
        if (allPackages.isEmpty()) {
            allPackages.clear();
            allPackages.addAll(LocalRUtil.parseInstalledPackages());

//            if (hasChanged[0]) { // todo reestablish this
            // update spell checker
            if (ApplicationManager.getApplication() != null)
                ApplicationManager.getApplication().invokeLater(new Runnable() {
                    public void run() {
                        Project[] projects = ProjectManager.getInstance().getOpenProjects();
                        for (Project project : projects) {
                            if (project.isInitialized() && project.isOpen() && !project.isDefault()) {
                                SpellCheckerManager spellCheckerManager = SpellCheckerManager.getInstance(project);
                                EditableDictionary dictionary = spellCheckerManager.getUserDictionary();

                                for (RPackage rPackage : RPackageService.getInstance().allPackages) {
                                    dictionary.addToDictionary(rPackage.getName());
                                    dictionary.addToDictionary(rPackage.getFunctionNames());
                                }

                                DaemonCodeAnalyzer.getInstance(project).restart();
                            }
                        }
                    }
                });
        }

        return getInstance().allPackages;
    }


    @NotNull
    public List<RPackage> getContainingPackages(String functionName) {
        List<RPackage> funPackages = Lists.newArrayList();

        for (RPackage rPackage : getInstance().getPackages()) {
            if (rPackage.hasFunction(functionName)) {
                funPackages.add(rPackage);
            }
        }

        return funPackages;
    }


    @NotNull
    public Collection<RPackage> getDependencies(RPackage somePckge) {
        Collection<RPackage> deps = new HashSet<RPackage>();

        for (String dep : somePckge.getDependencyNames()) {
            RPackage depPckg = getByName(dep);
            if (depPckg == null)
                continue;

            deps.add(depPckg);
        }

        return deps;
    }


    @Nullable
    public RPackage getByName(String packageName) {
        for (RPackage aPackage : getInstance().getPackages()) {
            if (aPackage.getName().equals(packageName)) {
                return aPackage;
            }
        }

        return null;
    }


    public List<Function> getFunctionByName(String funName) {
        List<Function> funList = new ArrayList<Function>();

        for (RPackage aPackage : getInstance().getPackages()) {
            Function function = aPackage.getFunction(funName);

            if (function != null) {
                funList.add(function);
            }
        }

        return funList;
    }


    public static void setRepositories(@NotNull final List<String> defaultRepositories,
                                       @NotNull final List<String> userRepositories) {
        RPackageService service = getInstance();

        service.enabledRepositories.clear();
        service.enabledRepositories.addAll(defaultRepositories);
        service.userRepositories.clear();
        service.userRepositories.addAll(userRepositories);
    }


    // todo still needed
    public static void restartInspections() {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            public void run() {
                Project[] projects = ProjectManager.getInstance().getOpenProjects();
                for (Project project : projects) {
                    if (project.isInitialized() && project.isOpen() && !project.isDefault()) {
                        DaemonCodeAnalyzer.getInstance(project).restart();
                    }
                }
            }
        });
    }
}
