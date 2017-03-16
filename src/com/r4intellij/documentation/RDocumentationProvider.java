package com.r4intellij.documentation;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.r4intellij.RFileType;
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.RReferenceExpressionImpl;
import com.r4intellij.psi.api.*;
import com.r4intellij.settings.RSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.net.ConnectException;
import java.net.URL;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import static com.r4intellij.interpreter.RSkeletonGenerator.SKELETON_DIR_NAME;
import static com.r4intellij.packages.RHelperUtil.LOG;

/**
 * For local function definitions provide doc string documentation (using docstring)
 * For library functions use R help.
 */
public class RDocumentationProvider extends AbstractDocumentationProvider {


    static {
        startHelpServer(null);
    }

    private static Integer HELP_SERVER_PORT;


    @Nullable
    @Override
    public PsiElement getCustomDocumentationElement(@NotNull Editor editor, @NotNull PsiFile file, @Nullable PsiElement contextElement) {
//        return contextElement;
//        if(contextElement== null || contextElement.getText().trim().isEmpty()) {
//            return null;
//        }

        return contextElement;
    }


    @Nullable
    @Override
    public String generateDoc(PsiElement reference, @Nullable PsiElement identifier) {
        if (!(RFileType.INSTANCE.equals(reference.getContainingFile().getFileType()))) return null;

        if (reference instanceof RStringLiteralExpression) return null;
        // check if it's a library function and return help if it is


        if (identifier == null) return null;

        String elementText = identifier.getText();
        if (elementText.trim().isEmpty()) return null;


        // first guess : process locally defined function definitions
        if (!isLibraryElement(reference)) {
            if (reference instanceof RAssignmentStatement) {
                RPsiElement assignedValue = ((RAssignmentStatement) reference).getAssignedValue();

                if (assignedValue instanceof RFunctionExpression) {
                    String docString = ((RFunctionExpression) assignedValue).getDocStringValue();
                    return docString != null ? docString : "No doc-string found for locally defined function.";
                }
            }
        }

        // ease R documentation lookup by detecting package if possible
        String packageName = null;

        // if possible resolve package from namespace prefix
        if (reference instanceof RReferenceExpressionImpl &&
                ((RReferenceExpressionImpl) reference).getNamespace() != null) {
            packageName = ((RReferenceExpressionImpl) reference).getNamespace();

        }

        // resolve method package from script dependencies
        if (packageName == null) {
            List<RPackage> origins = resolvePckgFromContext(elementText, identifier.getContainingFile());
            if (!origins.isEmpty()) {
                packageName = Iterables.getLast(origins).getName();
            }
        }

        // also make sure that we can provide help in skeleton files
        if (packageName == null && isLibraryElement(reference)) {
            packageName = reference.getContainingFile().getVirtualFile().getName().replaceAll(".r$", "");
        }

        // run generic R help on symbol
        if (HELP_SERVER_PORT == null) try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

//        return getHelpForFunction(elementText, packageName);
        String htmlTrimmed = getHelpFromLocalHelpServer(elementText, packageName);
        if (htmlTrimmed != null) return htmlTrimmed;

        return null;
        //
//        return getHelpForFunction(elementText, packageName);
    }

    @Nullable
    private String getHelpFromLocalHelpServer(String elementText, String packageName) {
        try {
            // check if help server is alive and restart it if necessary
            try {
                new Scanner(new URL("http://127.0.0.1:" + HELP_SERVER_PORT)
                        .openStream(), "UTF-8").useDelimiter("\\A").next();
            } catch (ConnectException e) {
                e.printStackTrace();
                startHelpServer(null);
            }

            URL localHelpURL;

            if (packageName != null) {
                localHelpURL = new URL("http://127.0.0.1:" + HELP_SERVER_PORT + "/library/" + packageName + "/help/" + elementText);
            } else {
                localHelpURL = new URL("http://127.0.0.1:" + HELP_SERVER_PORT + "/library/NULL/help/" + elementText);
            }

            String htmlRaw = new Scanner(localHelpURL.openStream(), "UTF-8").useDelimiter("\\A").next();
            htmlRaw.indexOf("</head><body>");

            String htmlTrimmed = htmlRaw.substring(htmlRaw.indexOf("</head><body>") + 13, htmlRaw.length()).trim();

            // fix URLs
            htmlTrimmed = htmlTrimmed.replace("href=\"../../", "href=\"http://127.0.0.1:" + HELP_SERVER_PORT + "/library/");
            htmlTrimmed = htmlTrimmed.replace("00Index.html", "http://127.0.0.1:" + HELP_SERVER_PORT + "/library/" + packageName + "/html/00Index.html");


            return htmlTrimmed;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }


    public static void startHelpServer(@Nullable Integer userDefinedPort) {
        String interpreter = RSettings.getInstance().getInterpreterPath();

        if (interpreter == null) {
            return;
        }

        String scriptText = "cat( tools::startDynamicHelp(start = TRUE)); Sys.sleep(3600)";

        if (userDefinedPort != null) {
            String custPortOption = "options(help.ports = " + userDefinedPort + ")";

            scriptText = custPortOption + "; " + scriptText;
        }


        String[] getPckgsCmd = new String[]{interpreter, "--vanilla", "--quiet", "--slave", "-e", scriptText};

        try {
            final CapturingProcessHandler processHandler = new CapturingProcessHandler(new GeneralCommandLine(getPckgsCmd));
            ProcessOutput processOutput = processHandler.runProcess(1000, false);

            if (userDefinedPort == null) {
                HELP_SERVER_PORT = Integer.parseInt(processOutput.getStdout());
            } else {
                HELP_SERVER_PORT = userDefinedPort;
            }
        } catch (Throwable e) {
            LOG.info("Failed to run start help-server");
        }
    }


    public static boolean isLibraryElement(PsiElement element) {
        return element != null &&
                Strings.nullToEmpty(
                        element.getContainingFile().getVirtualFile().getCanonicalPath()
                ).contains(SKELETON_DIR_NAME);
    }


    private List<RPackage> resolvePckgFromContext(String functionName, PsiFile psiFile) {
        // todo add Rmd chunk support here
        // todo This should also take the position into account since not all import may be done in the script header
        if (!(psiFile instanceof RFile)) {
            return Lists.newArrayList();
        }

        List<String> importNames = ((RFile) psiFile).getImportedPackages();
        List<RPackage> imports = RPackageService.getInstance().resolveDependencies(importNames);

        // filter for those that contain given function
        return imports.stream().distinct().filter(p -> p.hasFunction(functionName)).collect(Collectors.toList());
    }
}
