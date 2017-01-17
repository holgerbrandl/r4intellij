/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages.indexer;

import com.intellij.openapi.diagnostic.Logger;
import com.r4intellij.packages.Function;
import com.r4intellij.packages.RPackage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;


/**
 * An index of the user's R installation
 *
 * @author Holger Brandl
 */
@Deprecated

public class LibIndex extends HashSet<RPackage> {

    private static final Logger log = Logger.getInstance("#PackageCache");

    private static final long serialVersionUID = 3817077163528389033L;


    public LibIndex(Collection<? extends RPackage> collection) {
        super(collection);
    }


    public LibIndex() {
    }


    public List<RPackage> getPackagesOfFunction(String funName) {
        List<RPackage> hitList = new ArrayList<RPackage>();

        for (RPackage aPackage : this) {
            if (aPackage.hasFunction(funName)) {
                hitList.add(aPackage);
            }
        }

        return hitList;
    }


    public List<Function> getFunctionByName(String funName) {
        List<Function> funList = new ArrayList<Function>();

        for (RPackage aPackage : this) {
            Function function = aPackage.getFunction(funName);
            if (function != null) {
                funList.add(function);
            }
        }

        return funList;
    }


    public RPackage getByName(String packageName) {
        for (RPackage aPackage : this) {
            if (aPackage.getName().equals(packageName)) {
                return aPackage;
            }
        }

        return null;
    }
}