package com.r4intellij.packages;


import com.google.common.collect.Lists;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.webcore.packaging.RepoPackage;
import com.r4intellij.RUtils;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.parser.ParserDelegator;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RepoUtils {

    private static final String R_PACKAGES_DEFAULT_REPOS = "r-packages/r-packages-default-repos.r";

    private static final String R_ALL_PACKAGES = "r-packages/r-packages-all.r";


    @NonNls
    private static final String CRAN_URL = "https://cran.r-project.org/web/packages/available_packages_by_name.html";

    private static final Pattern urlPattern = Pattern.compile("\".+\"");

    static TreeMap<String, String> namesToDetails;


    @NotNull
    public static List<RDefaultRepository> getDefaultRepositories() {
        final String output = RHelperUtil.getHelperOutput(R_PACKAGES_DEFAULT_REPOS);
        if (output != null) {
            return toDefaultPackages((output));
        }
        return Lists.newArrayList();
    }


    private static List<RDefaultRepository> toDefaultPackages(@NotNull final String output) {
        final List<String> urls = getURLs(output);
        final List<RDefaultRepository> repos = Lists.newArrayList();
        for (int i = 0; i < urls.size(); i++) {
            repos.add(new RDefaultRepository(urls.get(i), i + 1));
        }
        return repos;
    }


    @NotNull
    public static List<String> getCRANMirrors() {
        final ProcessOutput output = RUtils.getProcessOutput("getCRANmirrors()[,\"URL\"]");
        if (output != null && output.getExitCode() == 0) {
            return getURLs(output.getStdout());
        }
        return Lists.newArrayList();
    }


    @NotNull
    private static List<String> getURLs(@NotNull String stdout) {
        final List<String> reposURL = Lists.newArrayList();
        final Matcher matcher = urlPattern.matcher(stdout);
        while (matcher.find()) {
            reposURL.add(matcher.group().replace('\"', ' ').trim());
        }
        return reposURL;
    }


    @NotNull
    public static List<RepoPackage> loadAvailablePackages() {
        final List<String> args = getHelperRepositoryArguments();

        final RHelperUtil.RRunResult result = RHelperUtil.runHelperWithArgs(R_ALL_PACKAGES, args.toArray(new String[args.size()]));

        if (result == null || result.getExitCode() != 0) {
            return Lists.newArrayList();
        }

        final List<RepoPackage> packageList = Lists.newArrayList();

        final String[] splittedOutput = StringUtil.splitByLines(result.getStdOut());

        for (String line : splittedOutput) {
            final List<String> packageAttributes = StringUtil.split(line, LocalRUtil.ARGUMENT_DELIMETER);
            if (packageAttributes.size() >= 3) {

                RepoPackage repoPackage = new RepoPackage(packageAttributes.get(1).replace("\"", ""), packageAttributes.get(3).replace("\"", ""),
                        packageAttributes.get(2).replace("\"", ""));

                packageList.add(repoPackage);
            }
        }

        getPackageRepositoryDetails();

        return packageList;
    }


    @NotNull
    static List<String> getHelperRepositoryArguments() {
        final RPackageService service = RPackageService.getInstance();
        final List<String> args = Lists.newArrayList();

        args.add(String.valueOf(service.CRANMirror + 1));

        if (service.enabledRepositories.size() > 0) {
            args.add(String.valueOf(service.enabledRepositories.size()));

            for (String repository : service.enabledRepositories) {
                for (RDefaultRepository defaultRepository : getDefaultRepositories()) {
                    if (defaultRepository.getUrl().equals(repository)) {
                        args.add(String.valueOf(defaultRepository.getIndex()));
                    }
                }
            }
        } else {
            args.add(String.valueOf(1));
            args.add(String.valueOf(1));
        }
        args.addAll(service.userRepositories);
        return args;
    }


    public static Map<String, String> getPackageRepositoryDetails() {
        if (namesToDetails != null) return namesToDetails;
        namesToDetails = new TreeMap<String, String>();
        final HTMLEditorKit.ParserCallback callback = new HTMLEditorKit.ParserCallback() {
            public boolean inTable;
            HTML.Tag myTag;
            String myPackageName;


            @Override
            public void handleStartTag(HTML.Tag tag,
                                       MutableAttributeSet set,
                                       int i) {
                myTag = tag;
                if ("table".equals(myTag.toString())) {
                    inTable = true;
                }
            }


            @Override
            public void handleText(char[] data, int pos) {
                if (myTag != null && "a".equals(myTag.toString()) && inTable && myPackageName == null) {
                    myPackageName = String.valueOf(data);
                } else if (myTag != null && "td".equals(myTag.toString())) {
                    namesToDetails.put(myPackageName, String.valueOf(data));
                    myPackageName = null;
                }
            }
        };

        try {
            final URL repositoryUrl = new URL(CRAN_URL);
            final InputStream is = repositoryUrl.openStream();
            final Reader reader = new InputStreamReader(is);
            try {
                new ParserDelegator().parse(reader, callback, true);
            } catch (IOException e) {
                RHelperUtil.LOG.warn(e);
            } finally {
                reader.close();
            }
        } catch (IOException e) {
            RHelperUtil.LOG.warn("Couldn't get package details", e);
        }

        return namesToDetails;
    }
}
