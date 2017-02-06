/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages;

import com.google.common.collect.Sets;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;


/**
 * A cachable title of an r-package
 *
 * @author Holger Brandl
 */
public class RPackage implements Serializable {

    private static final long serialVersionUID = -6424246043802526706L;

    private final String packageName;
    private final String title;
    private final String packageVersion;
    private final Set<String> dependencies;
    private final Set<String> imports;

    private Set<PckgFunction> functions;
    private Set<PckgDataSet> dataSets;

    private String repoUrl;


    public RPackage(String packageName, String packageVersion, String title,
                    Set<String> dependencies, Set<String> imports) {
        this.packageName = packageName;
        this.packageVersion = packageVersion;
        this.title = title;
        this.dependencies = dependencies;
        this.imports = imports;
    }


    public void setFunctions(List<PckgFunction> functions) {
        this.functions = Sets.newHashSet(functions);
    }


    public void setDatSets(List<PckgDataSet> dataSets) {
        this.dataSets = Sets.newHashSet(dataSets);
    }


    public List<String> getFunctionNames() {
        return functions.stream().map(PckgFunction::getName).collect(Collectors.toList());
    }


    public List<String> getDataSetNames() {
        return dataSets.stream().map(PckgDataSet::getName).collect(Collectors.toList());
    }


    public String getName() {
        return packageName;
    }


    public String getVersion() {
        return packageVersion;
    }


    public Collection<PckgFunction> getFunctions() {
        return Collections.unmodifiableCollection(functions);
    }


    public boolean hasFunction(String funName) {
        return functions.stream().anyMatch(ds -> ds.getName().equals(funName));

    }


    public boolean hasDataSet(String setName) {
        return dataSets.stream().anyMatch(ds -> ds.getName().equals(setName));
    }


    public PckgFunction getFunction(String funName) {
        for (PckgFunction function : functions) {
            if (function.getName().equals(funName)) {
                return function;
            }
        }

        return null;
    }


    public boolean isDummy() {
        return functions.isEmpty();
    }


    @Override
    public String toString() {
        return packageName;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RPackage rPackage = (RPackage) o;

        if (!Objects.equals(packageName, rPackage.packageName)) return false;

        return true;
    }


    public Set<String> getImports() {
        return imports;
    }


    public Collection<String> getDependencies() {
        return dependencies;
    }


    @Override
    public int hashCode() {
        return packageName == null ? -1 : packageName.hashCode();
    }


    public void setRepoUrl(String repoUrl) {
        this.repoUrl = repoUrl;
    }


    public String getRepoUrl() {
        return repoUrl;
    }


    public String getTitle() {
        return title;
    }


//    @Override
//    public int compareTo(@NotNull Object o) {
//        return Ordering.natural().compare(hashCode(), o.hashCode());
//    }
}
