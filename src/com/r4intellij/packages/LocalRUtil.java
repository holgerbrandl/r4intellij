package com.r4intellij.packages;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.webcore.packaging.InstalledPackage;
import com.jgoodies.common.base.Preconditions;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author avesloguzova
 * @author holgerbrandl
 */
public final class LocalRUtil {

    private static final String R_INSTALLED_PACKAGES = "r-packages/r-packages-installed.r";

    public static final String ARGUMENT_DELIMETER = " ";

    public static final Set<String> basePackages = Sets.newHashSet("stats", "graphics", "grDevices", "utils", "datasets", "grid", "methods", "base");
    //    public static final Set<String> basePackages = Sets.newHashSet("base", "utils", "stats", "datasets", "graphics",
//            "grDevices", "grid", "methods", "tools", "parallel", "compiler", "splines", "tcltk", "stats4");


    public static boolean isPackageBase(@NotNull final InstalledPackage pkg) {
        return basePackages.contains(pkg.getName());
    }


    public static Set<RPackage> getInstalledPackages() {
        RHelperUtil.runHelperWithArgs(RHelperUtil.R_HELPER_INSTALL_TIDYVERSE);


        ArrayList<String> helperOutput = Lists.newArrayList(RHelperUtil.getHelperOutput("package_summaries.r").split("\n"));


        return Sets.newHashSet(Iterables.transform(helperOutput, new ParseDescriptorIntoPackage()));
    }


    static String getPackageVersion(String packageName) {
        String s = RHelperUtil.runCommand("cat(packageDescription('" + packageName + "')$Version)");
        Preconditions.checkNotBlank(s, "version is empty");
        return s;
    }


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


    private static class ParseDescriptorIntoPackage implements com.google.common.base.Function<String, RPackage> {

        @Override
        public RPackage apply(String s) {
            String[] splitLine = s.split("\t");

            Set<String> dependencies = Sets.<String>newHashSet((splitLine.length == 4 ? splitLine[3] : "").split(",")).
                    stream().filter(f -> !f.isEmpty()).
                    filter(f -> !Objects.equals(f, "NA")).
                    collect(Collectors.toSet());

            Set<String> imports = Sets.<String>newHashSet((splitLine.length == 5 ? splitLine[4] : "").split(",")).
                    stream().filter(f -> !f.isEmpty()).
                    filter(f -> !Objects.equals(f, "NA")).
                    collect(Collectors.toSet());

            return new RPackage(splitLine[0].trim(), splitLine[1].trim(), splitLine[2], dependencies, imports);
        }
    }
}
