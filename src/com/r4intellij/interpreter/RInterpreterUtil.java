package com.r4intellij.interpreter;

import com.google.common.collect.Iterables;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.r4intellij.RPsiUtils;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author Hogler Brandl
 */
public class RInterpreterUtil {

//    private static String[] WINDOWS_PATHS = {"C:\\Program Files\\"};
//    private static String[] UNIX_PATHS = {"/usr/local/bin", "/usr/bin"};
//    private static String[] MAC_PATHS = {"/Library/Frameworks/R.framework/Resources", "/System/Library/Frameworks/R.framework/Resources"};


    public static String suggestHomePath() {
        return Iterables.getFirst(suggestHomePaths(), "");
    }


    private static List<String> suggestHomePaths() {
        if (SystemInfo.isWindows) {
            // TODO Is R always on PATH in a default installation?
//            pathOptions.addAll(getInstallationFromPaths("R.exe", WINDOWS_PATHS));
            return getFromPathVariable();
        }

        // try to detect from environment (i.e. PATH) on non-windows systems
        try {
            String rFromPath = new CapturingProcessHandler(new GeneralCommandLine("which", "R"))
                    .runProcess(5 * RPsiUtils.MINUTE).getStdout().trim();

            if (!rFromPath.isEmpty()) {
                return Arrays.asList(rFromPath);
            }
        } catch (ExecutionException e) {
            e.printStackTrace();
        }


        // if not on PATH check common locations
        if (SystemInfo.isMac) {
            List<String> macosPathOptions = Arrays.asList("/Library/Frameworks/R.framework/Resources/bin/R", "/usr/local/bin/R");
            return filterPathOptions(macosPathOptions);

        } else if (SystemInfo.isUnix) {
            List<String> linuxPathOptions = Arrays.asList("/usr/bin/R");
            return filterPathOptions(linuxPathOptions);
        }

        return new ArrayList<>();
    }


    private static <T> List<String> filterPathOptions(List<String> pathOptions) {
        return pathOptions.stream()
                .filter(path -> new File(path).isFile() && new File(path).canExecute())
                .collect(Collectors.toList());

    }


    @Deprecated // caused issues https://github.com/holgerbrandl/r4intellij/issues/79
    private static ArrayList<String> getInstallationFromPaths(@NotNull final String scriptName, @NotNull final String[] paths) {
        final ArrayList<String> result = new ArrayList<String>();

        for (String path : paths) {

            // does not work in tests
            final VirtualFile rootVDir = LocalFileSystem.getInstance().findFileByPath(path);
            if (rootVDir != null) {
//                final VirtualFile rScript = rootVDir.findFileByRelativePath("bin/" + scriptName);
                final File rScript = findScript(scriptName, path);
                if (rScript != null) {
                    result.add(FileUtil.toSystemDependentName(rScript.getPath()));
                }
            }
        }

        return result;
    }


    private static File findScript(@NotNull String scriptName, String path) {
        try {
            Stream<Path> binDirs = Files.walk(Paths.get(path), FileVisitOption.FOLLOW_LINKS)
                    .filter(Files::isDirectory)
                    .filter(f -> f.getFileName().toString().equals("bin"));

            Stream<File> rStream = binDirs.flatMap(dir -> {
                try {
                    Predicate<Path> scriptPredicate = f -> f.getParent().getFileName().toString().equals("bin") && f.getFileName().toString().equals(scriptName);
                    return Files.walk(dir, FileVisitOption.FOLLOW_LINKS).filter(Files::isExecutable).filter(scriptPredicate);
                } catch (IOException e) {
                    e.printStackTrace();
                }
                return Stream.empty();
            }).map(Path::toFile);

            return rStream.findFirst().orElse(null);

        } catch (IOException e) {
            e.printStackTrace();
        }

        return null;
    }


    private static ArrayList<String> getFromPathVariable() {
        final ArrayList<String> result = new ArrayList<String>();

        final String path = System.getenv("PATH");
        for (String pathEntry : StringUtil.split(path, ";")) {
            if (pathEntry.startsWith("\"") && pathEntry.endsWith("\"")) {
                if (pathEntry.length() < 2) continue;
                pathEntry = pathEntry.substring(1, pathEntry.length() - 1);
            }
            File f = new File(pathEntry, "R.exe");
            if (f.exists()) {
                result.add(FileUtil.toSystemDependentName(f.getPath()));
            }
        }

        return result;
    }
}
