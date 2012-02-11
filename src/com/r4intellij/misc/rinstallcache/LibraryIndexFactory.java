/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.rinstallcache;

import com.intellij.openapi.diagnostic.Logger;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Allows to index a user's R installation to make functions and packages available for code completion, inspections, etc.
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


    private static void updateIndex(LibIndex LibIndex) {
        boolean hasChanged = false;

        List<String> installedPackages = getListOfInstalledPackages();
        Map<String, String> pckgsWithVersions = getPackageVersions(installedPackages);

        for (String pckgName : installedPackages) {
            RPackage indexedPckg = LibIndex.getByName(pckgName);
            String pckgVersion = pckgsWithVersions.get(pckgName);

            if (indexedPckg != null && (indexedPckg.isDummy() || pckgVersion.equals(indexedPckg.getVersion()))) {
                continue;
            }

            indexPackage(pckgName);
            hasChanged = true;


        }

        if (hasChanged) {
            CachingUtils.saveObject(LIB_INDEX, getCacheFile());
        }
    }


    private static void indexPackage(String packageName) {
        try {
            RPackage rPackage = buildPackageCache(packageName);
            LIB_INDEX.add(rPackage);

        } catch (Throwable t) {
            log.warn("Indexing of package '" + packageName + "'  failed. Adding dummy package...");
            String packageVersion = getPackageVersion(packageName);
            RPackage rPackage = new RPackage(packageName, new ArrayList<Function>(), packageVersion, new ArrayList<String>());
            LIB_INDEX.add(rPackage);
        }
    }


    private static File getCacheFile() {
        return new File(System.getProperty("user.home") + File.separator + "r4i_libcache.dat");
    }


    static RPackage buildPackageCache(String packageName) {
        log.info("rebuilding cache of " + packageName);
        System.err.println("rebuilding cache of " + packageName);

        HashMap<String, Function> api = new HashMap<String, Function>();


        String rawFunSigs = CachingUtils.evalRComand("library(" + packageName + "); print(\"----\");  lsf.str(\"package:" + packageName + "\")");
        String[] splitFunSignatures = rawFunSigs.split("----\"\n")[1].replace("\n  ", "").split("\n");
        List<String> funSigs = new ArrayList<String>();
        for (int i = 1; i < splitFunSignatures.length - 2; i++) {
            String curLine = splitFunSignatures[i];
            if (curLine.contains(" : ")) {
                funSigs.add(curLine);
            } else {
                funSigs.add(funSigs.remove(funSigs.size() - 1) + curLine);
            }
        }

        for (String nameAndSig : funSigs) {
            int nameSplitter = nameAndSig.indexOf(':');
            String funName = nameAndSig.substring(0, nameSplitter).trim();
            String funSignature = nameAndSig.substring(nameSplitter + 2, nameAndSig.length()).trim();

            api.put(funName, new Function(funName, funSignature));
        }
//
//        String funNameOutput = CachingUtils.evalRComand("library(" + packageName + "); paste(ls(\"package:" + packageName + "\"), collapse=';')");
//        Matcher matcher = Pattern.compile("1] \"(.*)\"").matcher(funNameOutput);
//        matcher.find();
//        Collection<String> funNames = Arrays.asList(matcher.group(1).split(";"));
//
//
        String[] rawDocStrings = CachingUtils.evalRComand("pckgDocu <-library(help = " + packageName + "); pckgDocu$info[[2]]").split("\n");
        List<String> docStrings = new ArrayList<String>();
        for (int i = 1; i < rawDocStrings.length - 2; i++) {
            Matcher curLineMatcher = Pattern.compile("] \"(.*)\"").matcher(rawDocStrings[i]);
            curLineMatcher.find();
            String curLine = curLineMatcher.group(1);

            if (!curLine.startsWith(" ")) {
                docStrings.add(curLine);
            } else {
                docStrings.add(docStrings.remove(docStrings.size() - 1) + " " + curLine.trim());
            }
        }

        for (String docString : docStrings) {
            int splitter = docString.indexOf(" ");
            String funName = docString.substring(0, splitter).trim();
            String fundDesc = docString.substring(splitter, docString.length()).trim();
            Function function = api.get(funName);
            if (function != null) {
                function.setShortDesc(fundDesc);
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
        Matcher matcher;
        String rawDeps = CachingUtils.evalRComand("library(tools); paste(pkgDepends(\"" + packageName + "\")$Depends, collapse=\",\")");
        matcher = Pattern.compile("1] \"(.*)\"", Pattern.DOTALL).matcher(rawDeps);
        List<String> cleanedDeps = new ArrayList<String>();
        if (matcher.find()) {
            String depCsvList = matcher.group(1);
            if (!depCsvList.isEmpty()) {

                String[] deps = depCsvList.split(",");
                for (String dep : deps) {
                    cleanedDeps.add(dep.contains(" ") ? dep.split(" ")[0] : dep);
                }
            }
        }
        return cleanedDeps;
    }


    public static Map<String, String> getPackageVersions(List<String> packageNames) {
        StringBuilder sb = new StringBuilder();

        for (String packageName : packageNames) {
            sb.append("pckgDocu <-library(help = " + packageName + ")$info[[1]]; paste(pckgDocu[grep(\"Version|Package:\", pckgDocu)], collapse='__');");
        }


        String packageInfo = CachingUtils.evalRComand(sb.toString());

        Matcher matcher = Pattern.compile("Package:[ ]*([A-z0-9]*)__Version:[ ]*([0-9.-]*)[\"]", Pattern.DOTALL).matcher(packageInfo);
        Map<String, String> pckgVersions = new TreeMap<String, String>();
        while (matcher.find()) {
            pckgVersions.put(matcher.group(1), matcher.group(2));
        }

        return pckgVersions;
//        return matcher.find() ? matcher.group(1) : null;

    }


    static String getPackageVersion(String packageName) {
        String packageInfo = CachingUtils.evalRComand("pckgDocu <-library(help = " + packageName + "); pckgDocu$info[[1]]");
        Matcher matcher = Pattern.compile("Version:[ ]*([0-9.-]*)").matcher(packageInfo);

        return matcher.find() ? matcher.group(1) : null;
    }


    static List<String> getListOfInstalledPackages() {
        String output = CachingUtils.evalRComand("library()$results[,1]");
//        System.err.println("output was " + output.getOutput());

        Pattern pattern = Pattern.compile("\"([A-z0-9]*)\"");
        Matcher matcher = pattern.matcher(output);

        List<String> installedPackages = new ArrayList<String>();
        while (matcher.find()) {
            installedPackages.add(matcher.group(1));
        }

        return installedPackages;
    }
}
