/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.rinstallcache;

import java.io.Serializable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


/**
 * A cached description of an r-package
 *
 * @author Holger Brandl
 */
public class RPackage implements Serializable {

    private final String packageName;
    private final Set<Function> functions;
    private final String packageVersion;

    public RPackage(String packageName, List<Function> functions, String packageVersion) {

        this.packageName = packageName;
        this.functions = new HashSet<Function>(functions);
        this.packageVersion = packageVersion;
    }

    public String getName() {
        return packageName;
    }


    public boolean hasFunction(String funName) {
        for (Function function : functions) {
            if (function.getFunName().equals(funName)) {
                return true;
            }
        }

        return false;
    }


    @Override
    public String toString() {
        return packageName + " (" + packageVersion + ")";
    }
}
