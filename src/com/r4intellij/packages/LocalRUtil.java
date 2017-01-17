package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.intellij.execution.ExecutionException;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.CatchingConsumer;
import com.intellij.webcore.packaging.InstalledPackage;
import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * @author avesloguzova
 */
public final class LocalRUtil {

    private static final String R_INSTALLED_PACKAGES = "r-packages/r-packages-installed.r";

    static final String ARGUMENT_DELIMETER = " ";

    private static final String R_PACKAGES_DETAILS = "r-packages/r-packages-details.r";

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


    static void fetchPackageDetails(@NotNull final String packageName, @NotNull final CatchingConsumer<String, Exception> consumer) {
        ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
            @Override
            public void run() {
                try {
                    final String details = loadPackageDetails(packageName);
                    consumer.consume(formatDetails(packageName, details));
                } catch (ExecutionException e) {
                    consumer.consume(e);
                }
            }
        });
    }


    private static String formatDetails(@NotNull final String packageName, @NotNull final String details) {
        final String[] splittedString = details.split("\t");
        StringBuilder builder = new StringBuilder("<html><head>    <style type=\"text/css\">        " +
                "p {            font-family: Arial,serif; font-size: 12pt; margin: 2px 2px        }    " +
                "</style></head><body style=\"font-family: Arial,serif; font-size: 12pt; margin: 5px 5px;\">");
        if (RepoUtils.namesToDetails.containsKey(packageName)) {
            builder.append(RepoUtils.namesToDetails.get(packageName));
            builder.append("<br/>");
        }
        if (splittedString.length == 3) {
            builder.append("<h4>Version</h4>");
            builder.append(splittedString[0]);
            builder.append("<br/>");
            builder.append("<h4>Depends</h4>");
            builder.append(splittedString[1]);
            builder.append("<br/>");
            builder.append("<h4>Repository</h4>");
            builder.append(splittedString[2]);
        }
        return builder.toString();
    }


    private static String loadPackageDetails(@NotNull final String packageName) throws ExecutionException {
        final List<String> args = RepoUtils.getHelperRepositoryArguments();
        args.add(0, packageName);

        final RHelperUtil.RRunResult result = RHelperUtil.runHelperWithArgs(R_PACKAGES_DETAILS, args.toArray(new String[args.size()]));
        if (result != null && result.getExitCode() == 0) {
            return result.getStdOut();
        } else {
            if (result == null) {
                throw new ExecutionException("Can't fetch package details.");
            }
            throw new RExecutionException("Can't fetch package details.", result.getCommand(), result.getStdOut(), result.getStdErr(),
                    result.getExitCode());
        }
    }


}
