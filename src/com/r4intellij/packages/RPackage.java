/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages;

import java.io.Serializable;
import java.util.*;


/**
 * A cachable description of an r-package
 *
 * @author Holger Brandl
 */
public class RPackage implements Serializable {

    private static final long serialVersionUID = -55199278816509760L;

    private final String packageName;
    private String description;
    private final String packageVersion;

    private final Set<Function> functions;
    private final Collection<String> dependencies;
    private String repoUrl;


    public RPackage(String packageName, List<Function> functions, String packageVersion, List<String> dependencies) {

        this.packageName = packageName;
        this.functions = new HashSet<Function>(functions);
        this.packageVersion = packageVersion;
        this.dependencies = dependencies;
    }


    public String getName() {
        return packageName;
    }


    public String getVersion() {
        return packageVersion;
    }


    public Collection<Function> getFunctions() {
        return Collections.unmodifiableCollection(functions);
    }


    public boolean hasFunction(String funName) {
        for (Function function : functions) {
            if (function.getFunName().equals(funName)) {
                return true;
            }
        }

        return false;
    }


    public Function getFunction(String funName) {
        for (Function function : functions) {
            if (function.getFunName().equals(funName)) {
                return function;
            }
        }

        return null;
    }


    public Collection<String> getDependencyNames() {
        return dependencies;
    }


    public boolean isDummy() {
        return functions.isEmpty();
    }


    @Override
    public String toString() {
        return packageName + " (" + packageVersion + ")";
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RPackage rPackage = (RPackage) o;

        if (!packageName.equals(rPackage.packageName)) return false;

        return true;
    }


    @Override
    public int hashCode() {
        return packageName.hashCode();
    }


    public void setRepoUrl(String repoUrl) {
        this.repoUrl = repoUrl;
    }


    public String getRepoUrl() {
        return repoUrl;
    }


    public String getDescription() {
        return description;
    }


    public void setDescription(String description) {
        this.description = description;
    }
}
