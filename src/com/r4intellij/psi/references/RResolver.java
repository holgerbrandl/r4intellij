package com.r4intellij.psi.references;

import com.intellij.openapi.module.impl.scopes.LibraryScope;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.FileIndexFacade;
import com.intellij.openapi.roots.ModifiableModelsProvider;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.roots.libraries.LibraryTablesRegistrar;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.search.ProjectScopeImpl;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RPsiUtils;
import com.r4intellij.interpreter.RInterpreterConfigurable;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.stubs.RAssignmentNameIndex;
import com.r4intellij.typing.RTypeProvider;
import com.r4intellij.typing.types.RType;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class RResolver {

    public static void addFromSkeletonsAndRLibrary(@NotNull final PsiElement element,
                                                   @NotNull final List<ResolveResult> result,
                                                   @NotNull final String... names) {
        for (String name : names) {
            RResolver.addFromProject(name, element.getProject(), result);
            RResolver.addFromLibrary(element, result, name, RInterpreterConfigurable.USER_SKELETONS);
            RResolver.addFromLibrary(element, result, name, RInterpreterConfigurable.R_SKELETONS);
            RResolver.addFromLibrary(element, result, name, RInterpreterConfigurable.R_LIBRARY);
        }
    }


    public static void addFromLibrary(@NotNull final PsiElement element,
                                      @NotNull final List<ResolveResult> result,
                                      @NotNull final String name,
                                      @NotNull final String libraryName) {
        Project project = element.getProject();
        LibraryTable libraryTable = LibraryTablesRegistrar.getInstance().getLibraryTable(project);
        final Library library = libraryTable.getLibraryByName(libraryName);
        if (library != null) {
            final Collection<RAssignmentStatement> assignmentStatements = RAssignmentNameIndex.find(name, project,
                    new LibraryScope(
                            project, library));
            for (RAssignmentStatement statement : assignmentStatements) {
                final PsiFile containingFile = statement.getContainingFile();
                final PsiElement assignee = statement.getAssignee();
                if (assignee == null || containingFile.getName() == null) continue;
                result.add(new PsiElementResolveResult(statement));
            }
        }
    }


    public static void addFromProject(String name,
                                      @NotNull final Project project,
                                      @NotNull final List<ResolveResult> results) {
        Collection<RAssignmentStatement> statements =
                RAssignmentNameIndex.find(name, project,
                        new ProjectScopeImpl(project, FileIndexFacade
                                .getInstance(project)));
        for (RAssignmentStatement statement : statements) {
            final PsiElement assignee = statement.getAssignee();
            if (assignee == null) continue;
            results.add(new PsiElementResolveResult(statement));
        }
    }


    //TODO: should we search in other libraries too?
    public static void resolveWithNamespace(@NotNull final Project project,
                                            String name,
                                            String namespace,
                                            @NotNull final List<ResolveResult> result) {
        final ModifiableModelsProvider modelsProvider = ModifiableModelsProvider.SERVICE.getInstance();
        final LibraryTable.ModifiableModel model = modelsProvider.getLibraryTableModifiableModel(project);
        final Library library = model.getLibraryByName(RInterpreterConfigurable.R_LIBRARY);
        if (library != null) {
            final VirtualFile[] files = library.getFiles(OrderRootType.CLASSES);
            for (VirtualFile child : files) {
                if (namespace.equals(child.getParent().getName())) {
                    final VirtualFile file = child.findChild(name + ".R");
                    if (file != null) {
                        final PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
                        final RAssignmentStatement[] statements = PsiTreeUtil.getChildrenOfType(psiFile, RAssignmentStatement.class);
                        if (statements != null) {
                            for (RAssignmentStatement statement : statements) {
                                final PsiElement assignee = statement.getAssignee();
                                if (assignee != null && assignee.getText().equals(name)) {
                                    result.add(new PsiElementResolveResult(assignee));
                                }
                            }
                        }
                    }
                }
            }
        }
    }


    public static void resolveWithoutNamespaceInFile(@NotNull final PsiElement element,
                                                     @NotNull final List<ResolveResult> result,
                                                     String... names) {
        for (String name : names) {
            resolveWithoutNamespaceInFile(element, name, result);
        }
    }


    public static void resolveWithoutNamespaceInFile(@NotNull final PsiElement element,
                                                     String name,
                                                     @NotNull final List<ResolveResult> result) {
        RBlockExpression rBlock = PsiTreeUtil.getParentOfType(element, RBlockExpression.class);
        while (rBlock != null) {
            final RAssignmentStatement[] statements = PsiTreeUtil.getChildrenOfType(rBlock, RAssignmentStatement.class);
            if (statements != null) {
                for (RAssignmentStatement statement : statements) {
                    final PsiElement assignee = statement.getAssignee();
                    if (assignee != null && assignee.getText().equals(name)) {
                        result.add(new PsiElementResolveResult(statement));
                    }
                }
            }
            rBlock = PsiTreeUtil.getParentOfType(rBlock, RBlockExpression.class);
        }
        RForStatement rLoop = PsiTreeUtil.getParentOfType(element, RForStatement.class);
        while (rLoop != null) {
            final RExpression target = rLoop.getTarget();
            if (name.equals(target.getName())) {
                result.add(new PsiElementResolveResult(target));
            }
            rLoop = PsiTreeUtil.getParentOfType(rLoop, RForStatement.class);
        }
        final RFunctionExpression rFunction = PsiTreeUtil.getParentOfType(element, RFunctionExpression.class);
        if (rFunction != null) {
            final RParameterList list = rFunction.getParameterList();
            for (RParameter parameter : list.getParameterList()) {
                if (name.equals(parameter.getName())) {
                    result.add(new PsiElementResolveResult(parameter));
                }
            }
        }
        final PsiFile file = element.getContainingFile();
        final RAssignmentStatement[] statements = PsiTreeUtil.getChildrenOfType(file, RAssignmentStatement.class);
        if (statements != null) {
            for (RAssignmentStatement statement : statements) {
                final PsiElement assignee = statement.getAssignee();
                if (assignee != null && assignee.getText().equals(name)) {
                    result.add(new PsiElementResolveResult(statement));
                }
            }
        }
    }


    public static void resolveNameArgument(@NotNull final PsiElement element,
                                           String elementName,
                                           @NotNull final List<ResolveResult> result) {
        RCallExpression callExpression = PsiTreeUtil.getParentOfType(element, RCallExpression.class);
        if (callExpression != null) {
            RFunctionExpression functionExpression = RPsiUtils.getFunction(callExpression);
            RParameterList parameterList = PsiTreeUtil.getChildOfType(functionExpression, RParameterList.class);
            if (parameterList != null) {
                for (RParameter parameter : parameterList.getParameterList()) {
                    String name = parameter.getName();
                    if (name != null && name.equals(elementName)) {
                        result.add(0, new PsiElementResolveResult(parameter));
                        return;
                    }
                }
            }
        }
    }


    // TODO: massive refactoring awaits!!!
    public static void resolveFunction(PsiElement myElement, String name, List<ResolveResult> result) {
        PsiElement parent = myElement.getParent();
        if (parent != null && parent instanceof RCallExpression) {
            RCallExpression call = ((RCallExpression) parent);
            List<RExpression> arguments = call.getArgumentList().getExpressionList();
            if (call.getExpression().equals(myElement) && !arguments.isEmpty()) {
                RExpression firstArgument = arguments.get(0);
                List<ResolveResult> myResult = new ArrayList<ResolveResult>();

                RResolver.resolveWithoutNamespaceInFile(myElement, name, myResult);
                if (myResult.isEmpty()) {
                    RResolver.addFromSkeletonsAndRLibrary(myElement, myResult, name);
                }

                for (ResolveResult resolveResult : myResult) {
                    PsiElement resolved = resolveResult.getElement();
                    if (resolved instanceof RAssignmentStatement) {
                        RPsiElement assignedValue = ((RAssignmentStatement) resolved).getAssignedValue();
                        if (assignedValue instanceof RFunctionExpression) {
                            RFunctionExpression function = ((RFunctionExpression) assignedValue);
                            List<RCallExpression> nestedCalls = PsiTreeUtil.getChildrenOfTypeAsList(function, RCallExpression.class);
                            for (RCallExpression nestedCall : nestedCalls) {
                                if ("UseMethod".equals(nestedCall.getExpression().getText())) {
                                    RType firstType = RTypeProvider.getType(firstArgument);
                                    List<String> s3Classes = firstType.getS3Classes();
                                    s3Classes.add("default");
                                    for (String s3Class : s3Classes) {
                                        String genericName = name + "." + s3Class;
                                        List<ResolveResult> genericResult = new ArrayList<ResolveResult>();

                                        RResolver.resolveWithoutNamespaceInFile(myElement, genericName, genericResult);
                                        if (genericResult.isEmpty()) {
                                            RResolver.addFromSkeletonsAndRLibrary(myElement, genericResult, genericName);
                                        }

                                        if (!genericResult.isEmpty()) {
                                            result.addAll(genericResult);
                                            return;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                result.addAll(myResult);
            }
        }
    }
}
