/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.rinstallcache;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class PackageCache extends HashSet<RPackage> {

//    private static final long serialVersionUID = -5861760522437813012L;

    public static void main(String[] args) throws IOException, InterruptedException {
//        System.err.println(getListOfInstalledPackages());
//        RPackage plyrFuns = buildPackageCache("tikzDevice");

//        System.err.println("plyrFuns");
//        HashSet<RPackage> libraryCache = getLibraryCache();
//        System.err.println("cached " + libraryCache.size() + " packages!");

        PackageCache libraryCache = getLibraryCache();
        System.err.println(libraryCache.getPackagesOfFunction("a_ply"));

//        File cacheFile = new File(System.getProperty("user.home") + File.separator + "r4i_libcache.dat");
//        CachingUtils.saveObject(libraryCache, cacheFile);
//        PackageCache cache = (PackageCache) CachingUtils.loadObject(cacheFile);
    }

    private static PackageCache packageCache;

    private PackageCache() {

    }


    public static PackageCache getLibraryCache() {
        if (packageCache == null) {

            // try to load the index from the cache
            File cacheFile = new File(System.getProperty("user.home") + File.separator + "r4i_libcache.dat");
            packageCache = (PackageCache) CachingUtils.loadObject(cacheFile);
            if (packageCache != null) return packageCache;

            // rebuild the cache
            packageCache = new PackageCache();

            try {
                List<String> installedPackages = getListOfInstalledPackages();
                for (String packageName : installedPackages) {
                    try {
                        RPackage rPackage = buildPackageCache(packageName);
                        packageCache.add(rPackage);
                    } catch (Throwable t) {
                        System.err.println("Indexing of package '" + packageName + "'  failed");
                    }
                }
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }

            CachingUtils.saveObject(packageCache, cacheFile);
        }

        return packageCache;
    }


    private static List<String> getListOfInstalledPackages() throws IOException, InterruptedException {
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


    private static RPackage buildPackageCache(String packageName) throws IOException, InterruptedException {
        System.err.println("rebuilding cache of " + packageName);
        String lineBreaker = "&&&&";

        String funNameOutput = CachingUtils.evalRComand("library(" + packageName + "); paste(ls(\"package:" + packageName + "\"), collapse=';')");
        Matcher matcher = Pattern.compile("1] \"(.*)\"").matcher(funNameOutput);
        matcher.find();
        Collection<String> funNames = Arrays.asList(matcher.group(1).split(";"));


        String output = CachingUtils.evalRComand("pckgDocu <-library(help = " + packageName + "); paste(pckgDocu$info[[2]], collapse=\"" + lineBreaker + "\")");
//        System.err.println("output was " + output);

        List<Function> api = new ArrayList<Function>();

        String curFunName = null, curFunDesc = "";
        matcher = Pattern.compile("1] \"(.*)\"").matcher(output);
        matcher.find();
        String funDescs = matcher.group(1);
        for (String docuLine : funDescs.split(lineBreaker)) {
            if (docuLine.startsWith(" ")) {
                curFunDesc += " " + docuLine.trim();
            } else {
                if (curFunName != null) {
                    if (funNames.contains(curFunName) &&
                            curFunName.matches("^[A-z.].*") &&
                            !curFunName.equals("function") &&
                            !curFunName.contains("<-") &&
                            !curFunName.startsWith("["))
                        api.add(new Function(curFunName, curFunDesc));
                }


                String[] splitLine = docuLine.replaceFirst(" ", "____").split("____");
                curFunName = splitLine[0];
                curFunDesc = splitLine.length == 2 ? splitLine[1].trim() : "";
            }
        }


        String packageVersion = CachingUtils.getPackageVersion(packageName);
        RPackage rPackage = new RPackage(packageName, api, packageVersion);

        // add function definitions
        StringBuilder getFunImplsCmd = new StringBuilder("library(" + packageName + ");\n");
        for (Function function : api) {
            String funName = function.getFunName();
            getFunImplsCmd.append("print(\"" + funName + "\"); if(is.function(try(" + funName + "))) {" + funName + ";} else{ NULL};\n");
        }

        File tmpScript = File.createTempFile("rplugin", "R");
        BufferedWriter out = new BufferedWriter(new FileWriter(tmpScript));
        out.write(getFunImplsCmd.toString());
        out.close();

        String funImpls = CachingUtils.evalRScript(tmpScript);
//        tmpScript.delete();

        matcher = Pattern.compile("1] \"(.*)\"\n(function.*)", Pattern.DOTALL).matcher(funImpls);
//        String[] splitFuns = funImpls.split("\n?\\[1] \"(.*)\"\n?");
        String[] splitFuns = funImpls.split("> print.*\n.*\n");

//        if(splitFuns.length != )

        for (int i = 0; i < api.size(); i++) {
            Function anApi = api.get(i);
            matcher.find();

            anApi.setFunSignature(splitFuns[i + 1]);
        }

        return rPackage;
    }


    public List<RPackage> getPackagesOfFunction(String funName) {
        List<RPackage> hitList = new ArrayList<RPackage>();

        for (RPackage aPackage : packageCache) {
            if (aPackage.hasFunction(funName)) {
                hitList.add(aPackage);
            }
        }

        return hitList;
    }
}
