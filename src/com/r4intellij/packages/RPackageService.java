package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.spellchecker.SpellCheckerManager;
import com.intellij.spellchecker.dictionary.EditableDictionary;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.r4intellij.interpreter.RSkeletonGenerator;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.r4intellij.packages.PackageServiceUtilKt.rebuildIndex;

/**
 * @author avesloguzova
 * @author holgerbrandl
 */

// i.e. ~/Library//Caches/IntelliJIdea2016.1/plugins-sandbox/config/options/rpackages.xml
//        /Users/brandl/Library/Preferences/IntelliJIdea2016.3/options/rpackages.xml

@SuppressWarnings("WeakerAccess")
@State(name = "RPackageService", storages = {@Storage(file = "rpackages.xml")})
public class RPackageService implements PersistentStateComponent<RPackageService> {

    public static final Logger LOG = Logger.getInstance("#" + RPackageService.class.getName());

    // private fields won't be serialized
    // see http://www.jetbrains.org/intellij/sdk/docs/basics/persisting_state_of_components.html
    private transient Set<RPackage> allPackages; // = Sets.newConcurrentHashSet();
//    public Set<RPackage> packages = ContainerUtil.newConcurrentSet();

    public int CRANMirror = 1;

    public List<String> enabledRepositories = Lists.newArrayList();

    public List<String> userRepositories = Lists.newArrayList();


    @Nullable
    @Override
    public RPackageService getState() {
        return this;
    }


    //todo synchronized is ugly here, but otherwise getInstance is not yet done when after its first invocation
    // and retriggers an index refresh


    public synchronized static RPackageService getInstance() {
        return ServiceManager.getService(RPackageService.class);
    }


    public static RPackageService getTestInstance() {
        RPackageService service = ServiceManager.getService(RPackageService.class);
        File indexFile = service.getLibIndexFile();

        if (indexFile.exists()) {
            service.allPackages = (Set<RPackage>) loadObject(indexFile);
        }

        if (service.allPackages == null) {
            System.err.print("building package index for testing... ");
            service.allPackages = Sets.newConcurrentHashSet();
//            service.refreshIndexCache();
            saveObject(service.allPackages, indexFile);
            System.err.println("Done");
        }

        return service;
    }


    public void loadSkeletonCache() {
        if (allPackages != null) {
            return;
        }

        //noinspection unchecked
        allPackages = (Set<RPackage>) loadObject(getLibIndexFile());

        //        http://stackoverflow.com/questions/6992608/why-there-is-no-concurrenthashset-against-concurrenthashmap
        if (allPackages == null) allPackages = Sets.newConcurrentHashSet();


        // update index (no fancy sync anymore because it's superfast anyway)
        // disabled because trigger after skeleton-refresh now
//        ApplicationManager.getApplication().executeOnPooledThread((Runnable) this::refreshIndex);
    }


    private File getLibIndexFile() {
        String skeletonsPath = RSkeletonGenerator.getSkeletonsPath();
        if (!new File(skeletonsPath).exists()) {
            new File(skeletonsPath).mkdirs();
        }

        return new File(skeletonsPath, "libindex.dat");
    }


    @Override
    public void loadState(RPackageService state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    static Object loadObject(File f) {
        try {
            FileInputStream fin = new FileInputStream(f);
            ObjectInputStream ois = new ObjectInputStream(fin);
            Object o = ois.readObject();
            ois.close();

            return o;
        } catch (Throwable e) {
            System.err.println("could not load R package index, error was:\n" + e);
        }

        return null;
    }


    static void saveObject(Object o, File f) {
        try {
            FileOutputStream fout = new FileOutputStream(f);
            ObjectOutputStream oos = new ObjectOutputStream(fout);
            oos.writeObject(o);
            oos.close();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }




    /**
     * @param packageNames Packages to be reindexd if version has changed or package has been installed since last indexing run.If non are provided all packages will be refreshed.
     */
    public void refreshIndexCache(Project project, String... packageNames) {
        rebuildIndex(project, packageNames);

        saveObject(allPackages, getLibIndexFile());

//        ApplicationManager.getApplication().invokeLater(new Runnable() {

//        if (ApplicationManager.getApplication() != null) {
//            Project[] projects = ProjectManager.getInstance().getOpenProjects();
//            for (Project project : projects) {
        if (project.isInitialized() && project.isOpen() && !project.isDefault()) {
            SpellCheckerManager spellCheckerManager = SpellCheckerManager.getInstance(project);
            EditableDictionary dictionary = spellCheckerManager.getUserDictionary();

            for (RPackage rPackage : RPackageService.getInstance().allPackages) {
                dictionary.addToDictionary(rPackage.getName());
                dictionary.addToDictionary(rPackage.getFunctionNames());
                dictionary.addToDictionary(rPackage.getDataSetNames());
            }

            DaemonCodeAnalyzer.getInstance(project).restart();
        }
//            });
//        }
    }


    public Set<RPackage> getPackages() {
        return allPackages;
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


    public List<RPackage> resolveDependencies(Collection<String> packageNames) {
        Iterable<RPackage> packages = packageNames.stream().
                map(this::getByName).
                filter(Objects::nonNull).
                collect(Collectors.toList());

        List<RPackage> dependencies = Lists.newArrayList(packages).stream().
                flatMap(f -> {
                    List<RPackage> resolvedImports = resolveDependencies(f.getDependencies());
                    resolvedImports.add(0, f);
                    return resolvedImports.stream();
                }).collect(Collectors.toList());

        // workaround for https://github.com/tidyverse/tidyverse/issues/40


        if (dependencies.stream().anyMatch(rpg -> rpg.getName().equals("tidyverse"))) {
            List<RPackage> tidyverseAttachments = Stream.of("magrittr", "stringr", "dplyr", "purrr", "readr", "tidyr", "tibble", "ggplot2").
                    map(p -> new RPackageService().getByName(p)).
                    filter(Objects::nonNull).collect(Collectors.toList());

            int tidyverse = dependencies.stream().map(RPackage::getName).collect(Collectors.toList()).indexOf("tidyverse");
            dependencies.addAll(tidyverse + 1, tidyverseAttachments);
        }

        return dependencies;
    }



    public List<String> findImportsFor(@NotNull PsiElement element) {
        List<String> importedPackages = ((RFile) element.getContainingFile()).getImportedPackages(element);

        return resolveDependencies(importedPackages).stream().map(RPackage::getName).collect(Collectors.toList());
    }
}
