/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.rinstallcache;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;


/**
 * A cached description of an r-package
 *
 * @author Holger Brandl
 */
public class RPackage {

    private String packageName;

    private Set<Function> functions = new HashSet<Function>();

    public RPackage(String packageName) {

        this.packageName = packageName;
    }

    public String getName() {
        return packageName;
    }

    public void addFunction(Function function) {
        functions.add(function);
    }

    public Set<Function> getFunctions() {
        return Collections.unmodifiableSet(functions);
    }

    public boolean hasFunction(String funName) {
        for (Function function : functions) {
            if (function.getFunName().equals(funName)) {
                return true;
            }
        }

        return false;
    }
}
