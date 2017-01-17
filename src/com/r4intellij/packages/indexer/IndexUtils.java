/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages.indexer;

import com.r4intellij.packages.Function;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import org.jetbrains.annotations.Nullable;

import java.util.*;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
@Deprecated

public class IndexUtils {

    public static List<Function> getFunctionByName(String funName, @Nullable Collection<RPackage> importedPackages) {

        RPackageService packageService = RPackageService.getInstance();

        if (packageService == null)
            return Collections.emptyList();

        // if the user didn't import anything do a global search todo: does this make sense???
        if (importedPackages == null) {
            importedPackages = packageService.getPackages();
        } else {
            importedPackages = addImportDependencies(importedPackages);
        }

        List<Function> funs = new ArrayList<Function>();
        for (RPackage importedPackage : importedPackages) {
            if (importedPackage.hasFunction(funName))
                funs.add(importedPackage.getFunction(funName));

        }

        return funs;
    }


    private static Collection<RPackage> addImportDependencies(Collection<RPackage> importedPackages) {
        RPackageService packageService = RPackageService.getInstance();

        Collection<RPackage> imPckgsWithDeps = new HashSet<RPackage>();

        for (RPackage importedPackage : importedPackages) {
            imPckgsWithDeps.add(importedPackage);
            imPckgsWithDeps.addAll(packageService.getDependencies(importedPackage));
        }

        return imPckgsWithDeps;
    }


    public static List<String> getFunctionNames(RPackage rPackage) {
        List<String> funNames = new ArrayList<String>();
        for (Function function : rPackage.getFunctions()) {
            funNames.add(function.getFunName());
        }

        return funNames;
    }
}
