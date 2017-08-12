/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.psi.PsiElement;
import com.intellij.spellchecker.SpellCheckerManager;
import com.intellij.spellchecker.dictionary.EditableDictionary;
import com.r4intellij.RFileType;
import com.r4intellij.psi.api.RFile;
import com.r4intellij.psi.references.RResolver;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Holger Brandl
 */
public class RIndexCache {

    // private fields won't be serialized
    // see http://www.jetbrains.org/intellij/sdk/docs/basics/persisting_state_of_components.html
    // making it non-static is causing the loss of all content after loadSkeletonCache was run
//    @Transient
    private Set<RPackage> allPackages; // = Sets.newConcurrentHashSet();
//    private transient static Set<RPackage> allPackages; // = Sets.newConcurrentHashSet();
//    public Set<RPackage> packages = ContainerUtil.newConcurrentSet();


    private static RIndexCache INSTANCE;


    public synchronized static RIndexCache getInstance() {
        if (INSTANCE == null) {
            INSTANCE = new RIndexCache();
            INSTANCE.loadSkeletonCache();
        }

        return INSTANCE;
    }


    private void loadSkeletonCache() {
        if (allPackages != null) {
            return;
        }


        //noinspection unchecked
        if (RSettings.hasInterpreter()) {
            allPackages = (Set<RPackage>) loadObject(getLibIndexFile());
        }

        //        http://stackoverflow.com/questions/6992608/why-there-is-no-concurrenthashset-against-concurrenthashmap
        if (allPackages == null) {
//            allPackages = Sets.newConcurrentHashSet();
            allPackages = Collections.emptySet();
        }

        // this would presevere the content also for a non-static
//        allPackages = Collections.unmodifiableSet(allPackages);
        //        allPackages = new HashSet<>(allPackages);
        allPackages = Sets.newConcurrentHashSet(allPackages);

        // update index (no fancy sync anymore because it's superfast anyway)
        // disabled because trigger after skeleton-refresh now
//        ApplicationManager.getApplication().executeOnPooledThread((Runnable) this::refreshIndex);
    }


    static Object loadObject(File f) {
        // /Users/brandl/.gradle/caches/modules-2/files-2.1/com.jetbrains.intellij.idea/ideaIC/2017.1/e6f4c74ac091e55d0ac4e2ad26d91c12a4f23592/ideaIC-2017.1/system/r_skeletons/1481726564/.libindex.dat
        try {
            FileInputStream fin = new FileInputStream(f);
            ObjectInputStream ois = new ObjectInputStream(fin);
            Object o = ois.readObject();
            ois.close();

            return o;
        } catch (Throwable e) {
            System.err.println("could not load R package index cache, error was:\n" + e);
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


    public Set<RPackage> getPackages() {
        return Collections.unmodifiableSet(allPackages);
    }


    @Deprecated
    public void removeUninstalled(List<String> noLongerInstalled) {
        List<RPackage> removed = noLongerInstalled.stream()
                .map(this::getByName)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        allPackages.removeAll(removed);
    }


    public void replaceAndCleanup(@NotNull List<RPackage> reindexed, Project project) {
        allPackages.removeAll(reindexed);
        allPackages.addAll(reindexed);

        boolean changed = !reindexed.isEmpty() || cleanup(project);

        if (changed) saveCache(project);
    }


    private void saveCache(Project project) {
        saveObject(new HashSet<>(allPackages), getLibIndexFile());
        //        ApplicationManager.getApplication().invokeLater(new Runnable() {

//        if (ApplicationManager.getApplication() != null) {
//            Project[] projects = ProjectManager.getInstance().getOpenProjects();
//            for (Project project : projects) {
        if (project.isInitialized() && project.isOpen() && !project.isDefault()) {
            SpellCheckerManager spellCheckerManager = SpellCheckerManager.getInstance(project);
            EditableDictionary dictionary = spellCheckerManager.getUserDictionary();

            for (RPackage rPackage : getPackages()) {
                dictionary.addToDictionary(rPackage.getName());
                dictionary.addToDictionary(rPackage.getFunctionNames());
                dictionary.addToDictionary(rPackage.getDataSetNames());
            }

            DaemonCodeAnalyzer.getInstance(project).restart();
        }
//            });

    }


    boolean cleanup(Project project) {
//         val libraryTable = LibraryTablesRegistrar.getInstance().getLibraryTable(project)
//         val library = libraryTable.getLibraryByName(LibraryUtil.R_SKELETONS)
//
//         if (library == null) {
//             RPackageService.LOG.error("Could not find skeleton library")
//             return
//         }
//
//         val indexCache = RIndexCache.getInstance()
//
//         // cache cleanup
//         val skeletonPckgs = library.getFiles(OrderRootType.CLASSES).map { f -> f.name.replaceFirst("[.][rR]$".toRegex(), "") }
//         val toBeRemoved = listOf<RPackage>().toMutableList()
//         for (cachePckg in indexCache.packages) {
//             if(!skeletonPckgs.contains(cachePckg.name)){
//                 toBeRemoved.add(cachePckg)
//             }
//         }
//
//
        // remove no longer present packages from index
        String skeletonsDirURL = RResolver.getSkeletonLibrary(project).getRootProvider().getUrls(OrderRootType.CLASSES)[0];
        File skeletonsDir = new File(toURL(skeletonsDirURL).getFile());
//        String skeletonsDir = RSkeletonGenerator.getSkeletonsPath();

        List<RPackage> removed = allPackages.stream().filter(rPackage -> {
            File skelFile = new File(skeletonsDir, rPackage.getName() + RFileType.DOT_R_EXTENSION);
            return !skelFile.exists();
        }).collect(Collectors.toList());

        allPackages.removeAll(removed);

        return !removed.isEmpty();
    }


    @NotNull
    private static URL toURL(String skeletonsDirURL) {
        try {
            return new URL(skeletonsDirURL);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
    }


    @Deprecated
    public static void getTestInstance() {
        RIndexCache indexCache = getInstance();
        File indexFile = indexCache.getLibIndexFile();

        if (indexFile.exists()) {
            indexCache.allPackages = (Set<RPackage>) loadObject(indexFile);
        }

        if (indexCache.allPackages == null) {
            System.err.print("building package index for testing... ");
            indexCache.allPackages = Sets.newConcurrentHashSet();
//            indexCache.refreshIndexCache();
            saveObject(new HashSet<>(indexCache.allPackages), indexFile);
            System.err.println("Done");
        }
    }


    private File getLibIndexFile() {
        String skeletonsPath = RSkeletonGenerator.getSkeletonsPath();
        if (!new File(skeletonsPath).exists()) {
            new File(skeletonsPath).mkdirs();
        }

        return new File(skeletonsPath, ".libindex.dat");
    }


    public boolean isReady() {
        return allPackages != null && !allPackages.isEmpty();
    }


    public List<String> findImportsFor(@NotNull PsiElement element) {
        List<String> importedPackages = ((RFile) element.getContainingFile()).getImportedPackages(element);

        return resolveDependencies(importedPackages).stream().map(RPackage::getName).collect(Collectors.toList());
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
                    map(this::getByName).
                    filter(Objects::nonNull).collect(Collectors.toList());

            int tidyverse = dependencies.stream().map(RPackage::getName).collect(Collectors.toList()).indexOf("tidyverse");
            dependencies.addAll(tidyverse + 1, tidyverseAttachments);
        }

        return dependencies;
    }


    @Nullable
    public RPackage getByName(String packageName) {
        for (RPackage aPackage : RIndexCache.getInstance().getPackages()) {
            if (aPackage.getName().equals(packageName)) {
                return aPackage;
            }
        }

        return null;
    }

}
