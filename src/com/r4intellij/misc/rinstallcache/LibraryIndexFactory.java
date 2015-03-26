/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.rinstallcache;

import com.google.common.base.Joiner;
import com.google.common.base.Predicate;
import com.google.common.base.Splitter;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.spellchecker.SpellCheckerManager;
import com.intellij.spellchecker.dictionary.EditableDictionary;
import com.jgoodies.common.base.Preconditions;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;


/**
 * Allows to index a user's R installation to make functions and packages available for code completion, inspections,
 * etc.
 *
 * @author Holger Brandl
 */
public class LibraryIndexFactory {

    private static final Logger log = Logger.getInstance("#PackageCache");

    private static LibIndex LIB_INDEX;


    public static void main(String[] args) throws IOException, InterruptedException {
//        System.err.println(getListOfInstalledPackages());
//        RPackage plyrPckg = LibraryIndexFactory.buildPackageCache("base");
//
//        System.err.println(" plyrPckg");
        HashSet<RPackage> libraryCache = LibraryIndexFactory.getLibraryCache();
        System.err.println("cached " + libraryCache.size() + " packages!");

//        PackageCache libraryCache = getLibraryCache();
//        System.err.println(libraryCache.getPackagesOfFunction("a_ply"));

//        File cacheFile = new File(System.getProperty("user.home") + File.separator + "r4i_libcache.dat");
//        CachingUtils.saveObject(libraryCache, cacheFile);
//        PackageCache cache = (PackageCache) CachingUtils.loadObject(cacheFile);
    }


    public static LibIndex getLibraryCache() {
        if (LIB_INDEX == null) {

            // try to load the index from the cache
            LIB_INDEX = (LibIndex) CachingUtils.loadObject(getCacheFile());
            if (LIB_INDEX == null) {
                LIB_INDEX = new LibIndex();
            }

            updateIndex(LIB_INDEX);
        }

        return LIB_INDEX;
    }


    private static void updateIndex(final LibIndex libIndex) {
        // install dplyr and stringr
        CachingUtils.evalRCmd(
                "if(!require(dplyr)) install.packages('dplyr', repos='http://cran.us.r-project.org');" +
                        "if(!require(stringr)) install.packages('stringr', repos='http://cran.us.r-project.org');"
        );
        final boolean[] hasChanged = {false};

        List<String> installedPackages = getListOfInstalledPackages();

        ExecutorService executorService = Executors.newFixedThreadPool(4);
        for (final String packageName : installedPackages) {

            executorService.submit(new Runnable() {
                @Override
                public void run() {
                    RPackage indexedPckg = libIndex.getByName(packageName);
                    String pckgVersion = getPackageVersion(packageName);

                    if (indexedPckg != null && (indexedPckg.isDummy() || pckgVersion.equals(indexedPckg.getVersion()))) {
                        return;
                    }

                    RPackage indexedPackage = indexPackage(packageName);
                    libIndex.remove(libIndex.getByName(packageName));
                    libIndex.add(indexedPackage);

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
//        for (String packageName : installedPackages) {
//            RPackage indexedPckg = libIndex.getByName(packageName);
//            String pckgVersion = getPackageVersion(packageName);
//
//            if (indexedPckg != null && (indexedPckg.isDummy() || pckgVersion.equals(indexedPckg.getVersion()))) {
//                continue;
//            }
//
//            RPackage indexedPackage = indexPackage(packageName);
//            libIndex.remove(libIndex.getByName(packageName));
//            libIndex.add(indexedPackage);
//
//            hasChanged[0] = true;
//        }

        if (hasChanged[0]) {
            CachingUtils.saveObject(LIB_INDEX, getCacheFile());

            if (ApplicationManager.getApplication() != null)
                ApplicationManager.getApplication().invokeLater(new Runnable() {
                    public void run() {
                        Project[] projects = ProjectManager.getInstance().getOpenProjects();
                        for (Project project : projects) {
                            if (project.isInitialized() && project.isOpen() && !project.isDefault()) {
                                SpellCheckerManager spellCheckerManager = SpellCheckerManager.getInstance(project);
                                EditableDictionary dictionary = spellCheckerManager.getUserDictionary();

                                for (RPackage rPackage : LIB_INDEX) {
                                    dictionary.addToDictionary(rPackage.getName());
                                    dictionary.addToDictionary(IndexUtils.getFunctionNames(rPackage));
                                }

                                DaemonCodeAnalyzer.getInstance(project).restart();
                            }
                        }
                    }
                });
        }
    }


    private static RPackage indexPackage(String packageName) {
        RPackage indexedPackage;
        try {
            indexedPackage = buildPackageCache(packageName);

        } catch (Throwable t) {
            log.warn("Indexing of package '" + packageName + "'  failed. Adding dummy package...");
            t.printStackTrace();
            String packageVersion = getPackageVersion(packageName);
            assert !Strings.isNullOrEmpty(packageName);
            indexedPackage = new RPackage(packageName, new ArrayList<Function>(), packageVersion, new ArrayList<String>());
        }

        return indexedPackage;
    }


    private static File getCacheFile() {
        return new File(System.getProperty("user.home") + File.separator + "r4i_libcache.dat");
    }


    static RPackage buildPackageCache(final String packageName) {
        log.info("rebuilding cache of " + packageName);
        System.err.println("rebuilding cache of " + packageName);

        HashMap<String, Function> api = new HashMap<String, Function>();
        // note make sure to have a linebreak at the end of the output. otherwise the streamgobbler will not pick it up
//        String allFunsConcat = CachingUtils.evalRCommandCat("ls(getNamespace(\"" + packageName + "\"), all.names=F)");
        String allFunsConcat = CachingUtils.evalRCommandCat("getNamespaceExports('" + packageName + "')");

        List<String> allFuns = Splitter.on("\n").trimResults().splitToList(allFunsConcat);
        allFuns = Lists.newArrayList(Iterables.filter(allFuns, new Predicate<String>() {
            @Override
            public boolean apply(String funName) {
                return !funName.contains("<-") && !funName.startsWith(".");
            }
        }));


        if (allFuns.isEmpty()) {
            System.err.println("could not detect functions in package:" + packageName);
        }

        com.google.common.base.Function<String, String> quoteFun = new com.google.common.base.Function<String, String>() {
            public String apply(String funName) {
                // paste('toggleProbes', paste(deparse(args(AnnotationDbi::toggleProbes)), collapse=""), sep='||')
                return "cat(paste('" + funName + "', paste(deparse(args(" + packageName + "::" + funName + ")), collapse=''), sep='----'), fill=1)";
//                return "paste(" + funName + ", args(" + packageName + "::" + funName + "), sep='||');";
            }
        };
//        allFuns = allFuns.subList(5, 92);
//
//        String getFunSigs =Joiner.on(";").join(Lists.transform(allFuns, quoteFun));


//        String getFunSigs = "sigfuns <- c(" + Joiner.on(",").join(allFuns) + ");" +
//                "beautify_args <- function(name) { paste(deparse(substitute(name)), deparse(args(name)), collapse=\"\") }; " +
//                "beautify_args(sigfuns)";
//        System.err.println(getFunSigs);


        for (List<String> funNamesBatch : Lists.partition(allFuns, 50)) {
            String getFunSigs = Joiner.on(";").join(Lists.transform(funNamesBatch, quoteFun));
            String funSigs = CachingUtils.evalRCommand(getFunSigs);

            List<String> strings = Splitter.on("\n").trimResults().splitToList(funSigs);

            for (String funSig : strings) {
                String[] splitSig = funSig.split("----");
                if (splitSig.length == 1)
                    continue;

                String funName = splitSig[0];
                String signature = splitSig[1].replace("NULL", "").replace("\n", "");

                api.put(funName, new Function(funName, signature));

            }

        }

        //correct but too slow
//        for (String funName : allFuns) {
//            String funSig = CachingUtils.evalRCommand("args(" + packageName + "::" + funName + ")");
//            funSig = funSig.replace("NULL", "").replace("\n", "");
//            if (funSig.isEmpty()) {
//                continue;
//            }
//
//            api.put(funName, new Function(funName, funSig));
//        }

//        String rawFunSigs = CachingUtils.evalRCommand("library(" + packageName + "); print('----');  lsf.str('package:" + packageName + "')");
//        String[] splitFunSignatures = rawFunSigs.split("----\"\n")[1].replace("\n  ", "").split("\n");
//        List<String> funSigs = new ArrayList<String>();
//        for (int i = 1; i < splitFunSignatures.length - 2; i++) {
//            String curLine = splitFunSignatures[i];
//            if (curLine.contains(" : ")) {
//                funSigs.add(curLine);
//            } else {
//                funSigs.add(funSigs.remove(funSigs.size() - 1) + curLine);
//            }
//        }
//
//        for (String nameAndSig : funSigs) {
//            int nameSplitter = nameAndSig.indexOf(':');
//            String funName = nameAndSig.substring(0, nameSplitter).trim();
//            String funSignature = nameAndSig.substring(nameSplitter + 2, nameAndSig.length()).trim();
//
//            api.put(funName, new Function(funName, funSignature));
//        }

        String[] rawDocStrings = CachingUtils.evalRCommand("pckgDocu <-library(help = " + packageName + "); pckgDocu$info[[2]]").split("\n");
        List<String> fusedDocStrings = new ArrayList<String>();
        String curGroup = null;
        for (int i = 0; i < rawDocStrings.length - 2; i++) {
            String curRawDoc = rawDocStrings[i];
            String curLine = curRawDoc.substring(curRawDoc.indexOf("\""), curRawDoc.length());

            curLine = curLine.replace("\"", "").replace("\n", "");

            if (curLine.startsWith("     ")) {
                curGroup += curLine.trim();
            } else {
                if (!Strings.isNullOrEmpty(curGroup)) {
                    fusedDocStrings.add(curGroup);
                }
                curGroup = curLine.trim();
            }
        }

        for (String docString : fusedDocStrings) {
            int splitter = docString.indexOf(" ");

            if (splitter < 0) {
                System.err.println("doc string parsing failed for: " + docString);
            }

            String funName = docString.substring(0, splitter).trim();
            String fundDesc = docString.substring(splitter, docString.length()).trim();
            Function function = api.get(funName);
            if (function != null) {
                function.setShortDesc(fundDesc);
            } else {
                System.err.println("could not find function for " + funName);
            }

        }
//
//
//        String curFunName = null, curFunDesc = "";
//        matcher = Pattern.compile("1] \"(.*)\"").matcher(output);
//        matcher.find();
//        String funDescs = matcher.group(1);
//        for (String docuLine : funDescs.split(lineBreaker)) {
//            if (docuLine.startsWith(" ")) {
//                curFunDesc += " " + docuLine.trim();
//            } else {
//                if (curFunName != null) {
//                    if (funNames.contains(curFunName) &&
//                            curFunName.matches("^[A-z.].*") &&
//                            !curFunName.equals("function") &&
//                            !curFunName.contains("<-") &&
//                            !curFunName.startsWith("["))
//                        api.add(new Function(curFunName, curFunDesc));
//                }
//
//
//                String[] splitLine = docuLine.replaceFirst(" ", "____").split("____");
//                curFunName = splitLine[0];
//                curFunDesc = splitLine.length == 2 ? splitLine[1].trim() : "";
//            }
//        }

        // compile dependency list
        List<String> cleanedDeps = getDependencies(packageName);


        String packageVersion = getPackageVersion(packageName);
        RPackage rPackage = new RPackage(packageName, new ArrayList<Function>(api.values()), packageVersion, cleanedDeps);

//        // add function definitions
//        StringBuilder getFunImplsCmd = new StringBuilder("library(" + packageName + ");\n");
//        for (Function function : api) {
//            String funName = function.getFunName();
//            getFunImplsCmd.append("print(\"" + funName + "\"); if(is.function(try(" + funName + "))) {" + funName + ";} else{ NULL};\n");
//        }
//
//        File tmpScript = File.createTempFile("rplugin", "R");
//        BufferedWriter out = new BufferedWriter(new FileWriter(tmpScript));
//        out.write(getFunImplsCmd.toString());
//        out.close();
//
//        String funImpls = CachingUtils.evalRScript(tmpScript);
////        tmpScript.delete();
//
//        matcher = Pattern.compile("1] \"(.*)\"\n(function.*)", Pattern.DOTALL).matcher(funImpls);
////        String[] splitFuns = funImpls.split("\n?\\[1] \"(.*)\"\n?");
//        String[] splitFuns = funImpls.split("> print.*\n.*\n");
//
////        if(splitFuns.length != )
//
//        for (int i = 0; i < api.size(); i++) {
//            Function anApi = api.get(i);
//            matcher.find();
//
//            anApi.setFunSignature(splitFuns[i + 1]);
//        }


        return rPackage;
    }


    private static List<String> getDependencies(String packageName) {
        String rawDeps = CachingUtils.evalRCommandCat("library(tools); paste(pkgDepends('" + packageName + "')$Depends, collapse='')");
        return Splitter.on(" ").trimResults().splitToList(rawDeps);
    }


    static String getPackageVersion(String packageName) {
        String s = CachingUtils.evalRCommandCat("packageDescription('" + packageName + "')$Version");
        Preconditions.checkNotBlank(s, "version is empty");
        return s;
    }


    static List<String> getListOfInstalledPackages() {
        String output = CachingUtils.evalRCommandCat("library()$results[,1]");

        assert !Strings.isNullOrEmpty(output);

        return Splitter.on("\n").trimResults().splitToList(output);

//        Pattern pattern = Pattern.compile("\"([A-z0-9]*)\"");
//        Matcher matcher = pattern.matcher(output);
//
//        List<String> installedPackages = new ArrayList<String>();
//        while (matcher.find()) {
//            installedPackages.add(matcher.group(1));
//        }
//z
//        return installedPackages;
    }
}
