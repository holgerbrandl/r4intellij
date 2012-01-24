/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.rinstallcache;

import com.intellij.openapi.components.ServiceManager;
import com.r4intellij.psi.RFile;
import com.r4intellij.psi.RFuncall;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class RCacheUtils {

    public static List<Function> getFunctionByName(String funName, @Nullable Collection<RPackage> importedPackages) {
        PackageCache packageIndex = getPackageIndex();
        if (packageIndex == null)
            return Collections.emptyList();

        if (importedPackages == null) {
            importedPackages = packageIndex;
        }

        List<Function> funs = new ArrayList<Function>();
        for (RPackage importedPackage : importedPackages) {
            if (importedPackage.hasFunction(funName))
                funs.add(importedPackage.getFunction(funName));

        }

        return funs;
    }


    public static List<String> getImportedPackageNames(RFile file) {
        List<String> impPckgs = new ArrayList<String>();

        for (RFuncall libraryStatement : file.getImportStatements()) {
            //todo be more precie here and support library better
            String importedPackage = libraryStatement.getSublist().getSubList().get(0).getText();

            impPckgs.add(importedPackage);

        }
        return impPckgs;
    }

    public static List<RPackage> getImportedPackages(RFile file) {
        List<RPackage> importedPackages = new ArrayList<RPackage>();

        PackageCache packageIndex = getPackageIndex();
        if (packageIndex == null) // not yet loaded
            return importedPackages;

        for (String packageName : getImportedPackageNames(file)) {
            importedPackages.add(packageIndex.getByName(packageName));
        }

        return importedPackages;
    }

    public static PackageCache getPackageIndex() {
        PackageCacheService cacheService = ServiceManager.getService(PackageCacheService.class);
        return cacheService.getCache();
    }
}
