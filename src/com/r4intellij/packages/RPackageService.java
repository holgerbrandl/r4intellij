package com.r4intellij.packages;

import com.google.common.base.Splitter;
import com.google.common.collect.Iterables;
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
import com.intellij.spellchecker.SpellCheckerManager;
import com.intellij.spellchecker.dictionary.EditableDictionary;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.apache.commons.lang.ObjectUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

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


    public static RPackageService getInstance() {
        RPackageService service = ServiceManager.getService(RPackageService.class);
        service.loadPcgIndex();
        return service;
    }


    // or use startupactivity https://www.cqse.eu/en/blog/intellij-plugin-tutorial/
    private void loadPcgIndex() {
        if (allPackages != null) {
            return;
        }


        allPackages = Sets.newConcurrentHashSet();

        // trigger index update
        // update index (no fancy sync anymore because it's superfast anyway)
        ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {

            @Override
            public void run() {

                // also load index
                allPackages = (Set<RPackage>) loadObject(libIndexFile);

//        http://stackoverflow.com/questions/6992608/why-there-is-no-concurrenthashset-against-concurrenthashmap
                if (allPackages == null) allPackages = Sets.newConcurrentHashSet();


                boolean hasChanged = updatePackages();
                if (hasChanged) {
                    saveObject(allPackages, libIndexFile);
                }
            }
        });
    }


    @Override
    public void loadState(RPackageService state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    private File libIndexFile = new File("libindex.dat");


    static Object loadObject(File f) {
        try {
            FileInputStream fin = new FileInputStream(f);
            ObjectInputStream ois = new ObjectInputStream(fin);
            Object o = ois.readObject();
            ois.close();

            return o;
        } catch (Throwable e) {
            System.err.println("could not load R package index");
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


    private boolean updatePackages() {
        RHelperUtil.runHelperWithArgs(RHelperUtil.R_HELPER_INSTALL_TIDYVERSE);

        Set<RPackage> installedPackages = LocalRUtil.getInstalledPackages();

        ExecutorService executorService = Executors.newFixedThreadPool(8);

        final boolean[] hasChanged = {false};


        for (final RPackage rPackage : installedPackages) {
            final RPackage indexPackage = getByName(rPackage.getName());

            if (indexPackage != null && ObjectUtils.equals(indexPackage.getVersion(), rPackage.getVersion())) {
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
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
//        allPackages.clear();
//        allPackages.addAll(installedPackages);


//        if (hasChanged[0]) {
        if (ApplicationManager.getApplication() != null) {
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


//        final Set<RPackage> indexPackages = getPackages();

        // just takes 5 secs so why bother with syncing
//        ExecutorService executorService = Executors.newFixedThreadPool(4);
//
//        // remove packages that are no longer installed
//
//        Predicate<RPackage> isInstalled = new Predicate<RPackage>() {
//            @Override
//            public boolean apply(RPackage rPackage) {
//                return installedPackages.contains(rPackage);
//            }
//        };
//
//        final boolean[] hasChanged = {false};
//
//        for (final RPackage rPackage : installedPackages) {
//
//            executorService.submit(new Runnable() {
//                @Override
//                public void run() {
//                    RPackage indexedPckg = getByName(rPackage.getName());
//                    String pckgVersion = rPackage.getVersion();
//
//
//                    if (indexedPckg != null && pckgVersion.equals(indexedPckg.getVersion())) {
//                        return;
//                    }
//
//                    allPackages.remove(indexedPckg);
//                    allPackages.add(rPackage);
//
//                    hasChanged[0] = true;
//                }
//            });
//        }
//
//        executorService.shutdown();
//        try {
//            executorService.awaitTermination(1, TimeUnit.MINUTES);
//        } catch (InterruptedException e) {
//            e.printStackTrace();
//        }

        return hasChanged[0];
    }


    private void reindexPackage(RPackage rPackage, RPackage indexPackage) {
        // replace package

        RHelperUtil.LOG.info("detecting methods in " + rPackage.getName());

        String allFunsConcat = RHelperUtil.runCommand("cat(getNamespaceExports('" + rPackage.getName() + "'))").trim();
        List<String> allFuns = Splitter.on(" ").trimResults().splitToList(allFunsConcat);

        List<Function> functions = Lists.transform(allFuns, new com.google.common.base.Function<String, Function>() {
            @Override
            public Function apply(String s) {
                return new Function(s, "NA");
            }
        });

        rPackage.setFunctions(functions);

        if (indexPackage != null) {
            allPackages.remove(indexPackage);
        }

        allPackages.add(rPackage);
    }


    public Set<RPackage> getPackages() {
        return allPackages;
    }


    @NotNull
    public List<RPackage> getContainingPackages(String functionName) {
        List<RPackage> funPackages = Lists.newArrayList();

        for (RPackage rPackage : getPackages()) {
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


    public Set<RPackage> resolveDependencies(Collection<String> packageNames) {
        // remove empty and null package names from list
//        if(packageNames==null) packageNames = Lists.newArrayList();

//        packageNames = packageNames.stream().
//                filter(Objects::nonNull).
//                filter(f->!f.isEmpty()).collect(Collectors.toSet());

        Iterable<RPackage> packages = packageNames.stream().
                map(this::getByName).
                filter(Objects::nonNull).
                collect(Collectors.toList());

        List<RPackage> dependencies = Lists.newArrayList(packages).stream().
                flatMap(f -> resolveDependencies(f.getImports()).stream()).collect(Collectors.toList());


        return Sets.newHashSet(Iterables.concat(packages, dependencies));
    }
}
