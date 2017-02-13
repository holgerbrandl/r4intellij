package com.r4intellij.packages;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.webcore.packaging.InstalledPackage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static com.r4intellij.packages.RHelperUtil.*;

/**
 * @author avesloguzova
 * @author holgerbrandl
 */
public final class LocalRUtil {


    public static final String ARGUMENT_DELIMETER = " ";

    private static final PluginResourceFile RHELPER_PACKAGE_SUMMARIES = new PluginResourceFile("package_summaries.r");

    public static final Set<String> basePackages = Sets.newHashSet("stats", "graphics", "grDevices", "utils", "datasets", "grid", "methods", "base");

    //    public static final Set<String> basePackages = Sets.newHashSet("base", "utils", "stats", "datasets", "graphics",
//            "grDevices", "grid", "methods", "tools", "parallel", "compiler", "splines", "tcltk", "stats4");


    public static boolean isPackageBase(@NotNull final InstalledPackage pkg) {
        return basePackages.contains(pkg.getName());
    }


    /**
     * Fetch R package info including description and version.
     */
    public static Set<RPackage> getInstalledPackages() {
        String helperOutput = getHelperOutput(RHELPER_PACKAGE_SUMMARIES);

        if (helperOutput != null) {
            return Sets.newHashSet(Iterables.transform(Lists.newArrayList(helperOutput.split("\n")), new ParseDescriptorIntoPackage()));
        } else {
            return Sets.newHashSet();
        }
    }


    static String getPackageVersion(String packageName) {
        String version = runCommand("cat(packageDescription('" + packageName + "')$Version)");

        assert !isBlank(version) : "package version is empty";
        return version;
    }


    private static boolean isBlank(String s) {
        return Strings.nullToEmpty(s).trim().isEmpty();
    }


    public static List<PckgFunction> getFunctionByName(String funName, @Nullable Collection<RPackage> importedPackages) {

        RPackageService packageService = RPackageService.getInstance();

        if (packageService == null)
            return Collections.emptyList();

        // if the user didn't import anything do a global search todo: does this make sense???
        if (importedPackages == null) {
            importedPackages = packageService.getPackages();
        } else {
            importedPackages = addImportDependencies(importedPackages);
        }

        List<PckgFunction> funs = new ArrayList<PckgFunction>();
        for (RPackage importedPackage : importedPackages) {
            if (importedPackage.hasFunction(funName))
                funs.add(importedPackage.getFunction(funName));

        }

        return funs;
    }


    @Deprecated
    // use com.r4intellij.packages.RPackageService.resolveDependencies instead
    private static Collection<RPackage> addImportDependencies(Collection<RPackage> importedPackages) {
        RPackageService packageService = RPackageService.getInstance();

        Collection<RPackage> imPckgsWithDeps = new HashSet<RPackage>();

        for (RPackage importedPackage : importedPackages) {
            imPckgsWithDeps.add(importedPackage);
            imPckgsWithDeps.addAll(packageService.getDependencies(importedPackage));
        }

        return imPckgsWithDeps;
    }


    private static class ParseDescriptorIntoPackage implements com.google.common.base.Function<String, RPackage> {

        @Override
        public RPackage apply(String s) {
            // http://stackoverflow.com/questions/14602062/java-string-split-removed-empty-values
            String[] splitLine = s.split("\t", -1);

            if (splitLine.length != 5) {
                throw new RuntimeException("incorrect summary format in: " + s);
            }

            Set<String> dependencies = Sets.<String>newHashSet(splitLine[3].split(",")).
                    stream().filter(f -> !f.isEmpty()).
                    filter(f -> !Objects.equals(f, "NA")).
                    collect(Collectors.toSet());

            Set<String> imports = Sets.<String>newHashSet(splitLine[4].split(",")).
                    stream().filter(f -> !f.isEmpty()).
                    filter(f -> !Objects.equals(f, "NA")).
                    collect(Collectors.toSet());

            String packageName = splitLine[0].trim();
            String version = splitLine[1].trim();
            String title = splitLine[2];

            return new RPackage(packageName, version, title, dependencies, imports);
        }
    }
}
