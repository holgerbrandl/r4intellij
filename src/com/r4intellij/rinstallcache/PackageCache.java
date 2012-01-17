/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.rinstallcache;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class PackageCache extends HashSet<RPackage> {

    public static void main(String[] args) throws IOException, InterruptedException {
//        System.err.println(getListOfInstalledPackages());
        HashSet<RPackage> libraryCache = getLibraryCache();
        System.err.println("cached " + libraryCache.size() + " packages!");
    }


    private static PackageCache packageCache;

    private PackageCache() {

    }

    public static PackageCache getLibraryCache() {
        if (packageCache == null) {
            packageCache = new PackageCache();

            try {
                List<String> installedPackages = getListOfInstalledPackages();
                for (String packageName : installedPackages) {
                    packageCache.add(rebuildPackageCache(packageName));
                }
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
        }

        return packageCache;
    }

    private static RPackage rebuildPackageCache(String packageName) throws IOException, InterruptedException {
        List<String> funNames = getPackageAPI(packageName);

//        System.err.println("funlist: " + funNames.toString());

        // get detailed information for all functions
        String getFunImplsCmd = funNames.toString().replace("[", "").replace("[", "").replace(",", ";");
//        StreamGobbler outputGobbler = evalRCmd(getFunImplsCmd);
//        System.out.println("output was \n:"+ outputGobbler.getOutput());

        String output = CachingUtils.evalRCmd(getFunImplsCmd).getOutput();
        Pattern pattern = Pattern.compile("function.*");

        String[] funDefs = output.split("<environment: namespace:" + packageName + ">");
        RPackage rPackage = new RPackage(packageName);

        for (int i = 0; i < funDefs.length; i++) {
            rPackage.addFunction(new Function(funNames.get(i), funDefs[i]));
        }

        return rPackage;
    }

    private static List<String> getPackageAPI(String packageName) throws IOException, InterruptedException {
        String output = CachingUtils.evalRComand("paste(ls('package:" + packageName + "'), collapse=',')");
        System.err.println("output was " + output);

        Pattern pattern = Pattern.compile(" \"(.*)\"");
        Matcher matcher = pattern.matcher(output);
        matcher.find();
        return Arrays.asList(matcher.group(1).split(","));
    }

    private static List<String> getListOfInstalledPackages() throws IOException, InterruptedException {
        String output = CachingUtils.evalRComand("paste(search(), collapse=', ')");
//        System.err.println("output was " + output.getOutput());

        Pattern pattern = Pattern.compile("package:([^,]*),");
        Matcher matcher = pattern.matcher(output);

        List<String> installedPackages = new ArrayList<String>();
        while (matcher.find()) {
//                System.err.println("pacakge is "+ matcher.group(1));
            installedPackages.add(matcher.group(1));
        }

        return installedPackages;
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