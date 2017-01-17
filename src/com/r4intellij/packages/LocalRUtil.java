package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.webcore.packaging.InstalledPackage;
import com.jgoodies.common.base.Preconditions;
import com.r4intellij.packages.remote.RepoUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

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


    public static List<RPackage> parseInstalledPackages() {
        final ArrayList<RPackage> installedPackages = Lists.newArrayList();
        final String stdout = RHelperUtil.getHelperOutput(R_INSTALLED_PACKAGES);

        if (stdout == null) {
            return installedPackages;
        }


        // todo remove this hack and fetch package description from local installation
        Map<String, String> repositoryDetails = RepoUtils.getPackageRepositoryDetails();

        final String[] splittedOutput = StringUtil.splitByLines(stdout);

        for (String line : splittedOutput) {
            final List<String> packageAttributes = StringUtil.split(line, ARGUMENT_DELIMETER);

            if (packageAttributes.size() == 4) {
                RPackage rPackage = new RPackage(
                        packageAttributes.get(1).replace("\"", ""),
                        Lists.<Function>newArrayList(), packageAttributes.get(2).replace("\"", ""),
                        Lists.<String>newArrayList()
                );

                if (repositoryDetails.containsKey(rPackage.getName())) {
                    rPackage.setDescription(repositoryDetails.get(rPackage.getName()));
                }

                // add methods and dependencies
                installedPackages.add(rPackage);
            }
        }

        Collections.sort(installedPackages, new Comparator<RPackage>() {
            @Override
            public int compare(RPackage o1, RPackage o2) {
                return StringUtil.compare(o1.getName(), o2.getName(), true);
            }
        });

        return installedPackages;
    }


    static String getPackageVersion(String packageName) {
        String s = RHelperUtil.evalRCommandCat("packageDescription('" + packageName + "')$Version");
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
}
