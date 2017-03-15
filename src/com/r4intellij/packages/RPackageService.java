package com.r4intellij.packages;

import com.google.common.base.Splitter;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.psi.PsiElement;
import com.intellij.spellchecker.SpellCheckerManager;
import com.intellij.spellchecker.dictionary.EditableDictionary;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.r4intellij.interpreter.RSkeletonGenerator;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author avesloguzova
 * @author holgerbrandl
 */

// i.e. ~/Library//Caches/IntelliJIdea2016.1/plugins-sandbox/config/options/rpackages.xml
//        /Users/brandl/Library/Preferences/IntelliJIdea2016.3/options/rpackages.xml

@SuppressWarnings("WeakerAccess")
@State(name = "RPackageService", storages = {@Storage(file = "rpackages.xml")})
public class RPackageService implements PersistentStateComponent<RPackageService> {

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
        RPackageService service = ServiceManager.getService(RPackageService.class);
        service.loadPcgIndex();
        return service;
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
            service.refreshIndex();
            saveObject(service.allPackages, indexFile);
            System.err.println("Done");
        }

        return service;
    }


    // or use startupactivity https://www.cqse.eu/en/blog/intellij-plugin-tutorial/
    private void loadPcgIndex() {
        if (allPackages != null) {
            return;
        }

        //noinspection unchecked
        allPackages = (Set<RPackage>) loadObject(getLibIndexFile());

        //        http://stackoverflow.com/questions/6992608/why-there-is-no-concurrenthashset-against-concurrenthashmap
        if (allPackages == null) allPackages = Sets.newConcurrentHashSet();


        // update index (no fancy sync anymore because it's superfast anyway)
        ApplicationManager.getApplication().executeOnPooledThread((Runnable) this::refreshIndex);
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
     * @param packageNames Packages to be reindexd if version has changed or package has been installed since last
     *                     indexing run.If non are provided all packages will be refreshed.
     */
    public boolean refreshIndex(String... packageNames) {
        Set<RPackage> installedPackages = LocalRUtil.getInstalledPackages();

        // remove packages from index that are no longer present
        if (packageNames.length == 0) {
            Sets.SetView<RPackage> noLongerInstalled = Sets.difference(allPackages, installedPackages);
            allPackages.removeAll(noLongerInstalled);
        }

        // todo refresh most commons packages first for better ux

        // cut down packges to be refreshed to speed up calculations
//        if(packageNames.length>0){
//            installedPackages = installedPackages.stream().
//                    filter(p -> Arrays.asList(packageNames).contains(p.getName())).
//                    collect(Collectors.toSet());
//        }

        ExecutorService executorService = Executors.newFixedThreadPool(5);

        final boolean[] hasChanged = {false};

        // todo use ProgressManager.getInstance().run(new Task.Backgroundable ...

        for (final RPackage rPackage : installedPackages) {
            final RPackage indexPackage = getByName(rPackage.getName());

            if (indexPackage != null && Objects.equals(indexPackage.getVersion(), rPackage.getVersion())) {
                continue;
            }

            executorService.submit(new Runnable() {
                @Override
                public void run() {
                    reindexPackage(rPackage, indexPackage);
                    hasChanged[0] = true;
                }
            });
        }


        executorService.shutdown();
        try {
            executorService.awaitTermination(1, TimeUnit.DAYS);
            RHelperUtil.LOG.info("finished package indexing ");

        } catch (InterruptedException e) {
            e.printStackTrace();
        }
//        allPackages.clear();
//        allPackages.addAll(installedPackages);


        if (hasChanged[0]) {
            saveObject(allPackages, getLibIndexFile());

            if (ApplicationManager.getApplication() != null) {
                Project[] projects = ProjectManager.getInstance().getOpenProjects();
                for (Project project : projects) {
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
                }
            }
        }

        return hasChanged[0];
    }


    private void reindexPackage(RPackage rPackage, RPackage indexPackage) {
        // replace package

        RHelperUtil.LOG.info("detecting methods in " + rPackage.getName());


        // get all package functions
        String allFunsConcat = RHelperUtil.runCommand("cat(getNamespaceExports('" + rPackage.getName() + "'))").trim();
        List<String> allFuns = Splitter.on(" ").trimResults().splitToList(allFunsConcat);

        List<PckgFunction> functions = Lists.transform(allFuns, new com.google.common.base.Function<String, PckgFunction>() {
            @Override
            public PckgFunction apply(String funName) {
                return new PckgFunction(funName);
            }
        });

        rPackage.setFunctions(functions);

        //get all package datasets
        // pName="ggplot2"
        // cat(with(as.data.frame(data(package = pName)$result), paste(Item, Title, sep="$$$")), sep="\n")

        if (!Arrays.asList("base", "stats", "backports").contains(rPackage.getName())) {
            String dsCmd = "cat(with(as.data.frame(data(package = '" + rPackage.getName() + "')$result), paste(Item, Title, sep='$$$')), sep='\\n')";
            List<String> allDataConcat = Splitter.on("\n").trimResults().splitToList(RHelperUtil.runCommand(dsCmd));
            List<PckgDataSet> dataSets = allDataConcat.stream().filter(f -> !f.trim().isEmpty()).
                    map(line -> {
                        List<String> splitLine = Splitter.on("$$$").trimResults().splitToList(line);
                        return new PckgDataSet(splitLine.get(0), splitLine.get(1));
                    }).
                    collect(Collectors.toList());

            rPackage.setDatSets(dataSets);
        }

        if (indexPackage != null) {
            allPackages.remove(indexPackage);
        }

//        System.err.println("finished indexing of "+ rPackage.getName());
        allPackages.add(rPackage);
    }


    public Set<RPackage> getPackages() {
        return allPackages;
    }


    @NotNull
    public List<RPackage> getContainingPackages(String symbol) {
        return getPackages().stream().filter(pckg ->
                pckg.hasFunction(symbol) || pckg.hasDataSet(symbol)
        ).collect(Collectors.toList());
    }


    @NotNull
    public Collection<RPackage> getDependencies(RPackage somePckge) {
        Collection<RPackage> deps = new HashSet<RPackage>();

        for (String dep : somePckge.getDependencies()) {
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


    public List<PckgFunction> getFunctionByName(String funName) {
        List<PckgFunction> funList = new ArrayList<PckgFunction>();

        for (RPackage aPackage : getInstance().getPackages()) {
            PckgFunction function = aPackage.getFunction(funName);

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


    public Set<RPackage> getImporting(@NotNull RPackage rPackage) {
        return allPackages.stream().
                filter(p -> p.getImports().contains(rPackage.getName())).
                collect(Collectors.toSet());

    }


    @Deprecated
    public boolean isReady() {
        return !allPackages.isEmpty();
    }


    public List<RPackage> resolveImports(PsiElement psiElement) {
        List<String> importedPackages = ((RFile) psiElement.getContainingFile()).getImportedPackages();

        return resolveDependencies(importedPackages);
    }


    public void refreshIndexInThread() {
        ApplicationManager.getApplication().invokeLater(new Runnable() {

            @Override
            public void run() {
                refreshIndex();
            }
        });
    }
}
