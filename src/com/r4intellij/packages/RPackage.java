

package com.r4intellij.packages;

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

    private List<PckgFunction> functions = Collections.emptyList();
    private List<PckgDataSet> dataSets = Collections.emptyList();

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
        this.functions = new ArrayList<>(functions); // rewrap because provided list may not be serializable
    }


    public void setDatSets(List<PckgDataSet> dataSets) {
        this.dataSets = new ArrayList<>(dataSets); // rewrap because provided list may not be serializable
    }


    public List<String> getFunctionNames() {
        if (functions == null) return Collections.emptyList();
        return functions.stream().map(PckgFunction::getName).collect(Collectors.toList());
    }


    public List<String> getDataSetNames() {
        if (dataSets == null) return Collections.emptyList();
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


    @Deprecated
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

        return Objects.equals(packageName, rPackage.packageName);
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
