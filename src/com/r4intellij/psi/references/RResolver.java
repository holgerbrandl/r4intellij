package com.r4intellij.psi.references;

import com.google.common.collect.Iterables;
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
import com.r4intellij.packages.RPackage;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.stubs.RAssignmentNameIndex;
import com.r4intellij.settings.LibraryUtil;
import com.r4intellij.settings.RSettings;
import com.r4intellij.typing.RTypeProvider;
import com.r4intellij.typing.types.RType;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class RResolver {

    static void addFromSkeletonsAndRLibrary(@NotNull final PsiElement element,
                                            @NotNull final List<ResolveResult> result,
                                            @NotNull final String... names) {
        for (String name : names) {
            if (RSettings.getInstance().isResolveInModule()) {
                addFromProject(name, element.getProject(), result);
            }

//            addFromLibrary(element, result, name, LibraryUtil.USER_SKELETONS);

            // by design always resolve from user library
            addFromLibrary(element, result, name, LibraryUtil.R_LIBRARY);

            // too unspecific and does not reflect imports
//            addFromLibrary(element, result, name, RSettingsConfigurable.R_SKELETONS);

            addFromImports(element, result, name, LibraryUtil.R_SKELETONS);

        }
    }


    private static void addFromImports(@NotNull final PsiElement element,
                                       @NotNull final List<ResolveResult> result,
                                       @NotNull final String name,
                                       @NotNull final String libraryName) {

        // get all imports for the current File
        List<String> imports = RPackageService.getInstance().resolveImports(element).stream().
                map(RPackage::getName).collect(Collectors.toList());

        Project project = element.getProject();
        LibraryTable libraryTable = LibraryTablesRegistrar.getInstance().getLibraryTable(project);
        final Library library = libraryTable.getLibraryByName(libraryName);
        if (library == null) {
            return;
        }

        final Collection<RAssignmentStatement> assignmentStatements =
                RAssignmentNameIndex.find(name, project, new LibraryScope(project, library));

        for (RAssignmentStatement statement : assignmentStatements) {
            final PsiFile containingFile = statement.getContainingFile();

            if (!imports.contains(containingFile.getName().replaceAll(".r$", ""))) continue;

            final PsiElement assignee = statement.getAssignee();
            if (assignee == null || containingFile.getName() == null) continue;

            result.add(new PsiElementResolveResult(statement));
        }
    }


    private static void addFromLibrary(@NotNull final PsiElement element,
                                       @NotNull final List<ResolveResult> result,
                                       @NotNull final String name,
                                       @NotNull final String libraryName) {
        Project project = element.getProject();
        LibraryTable libraryTable = LibraryTablesRegistrar.getInstance().getLibraryTable(project);
        final Library library = libraryTable.getLibraryByName(libraryName);

        if (library == null) {
            return;
        }

        final Collection<RAssignmentStatement> assignmentStatements =
                RAssignmentNameIndex.find(name, project, new LibraryScope(project, library));

        for (RAssignmentStatement statement : assignmentStatements) {
            final PsiFile containingFile = statement.getContainingFile();
            final PsiElement assignee = statement.getAssignee();
            if (assignee == null || containingFile.getName() == null) continue;
            result.add(new PsiElementResolveResult(statement));
        }
    }


    private static void addFromProject(String name,
                                       @NotNull final Project project,
                                       @NotNull final List<ResolveResult> results) {

        //todo actually we should just resolve in module here and not in project
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
    //TODO: why don't we use the stub index here? should be faster and more elegant
    public static void resolveWithNamespace(@NotNull final Project project,
                                            String name,
                                            String namespace,
                                            @NotNull final List<ResolveResult> result) {
        final ModifiableModelsProvider modelsProvider = ModifiableModelsProvider.SERVICE.getInstance();
        final LibraryTable.ModifiableModel model = modelsProvider.getLibraryTableModifiableModel(project);
        final Library library = model.getLibraryByName(LibraryUtil.R_SKELETONS);

        if (library == null) {
            return;
        }
//            final VirtualFile[] files = library.getFiles(OrderRootType.CLASSES);
        // works for unit-tests but library layout is not the same as if IJ is running
//            Optional<VirtualFile> first = Arrays.stream(files).filter(f -> f.getName().equals(namespace + ".r")).findFirst();

//            if (!first.isPresent())
//                return;

//            for (VirtualFile child : files) {
//            final VirtualFile file = first.get();
//                final VirtualFile file = child.findChild(namespace + ".r");

        VirtualFile file = Arrays.stream(library.getFiles(OrderRootType.CLASSES)).
                map(d -> d.findFileByRelativePath(namespace + ".r")).
                filter(Objects::nonNull).findFirst().orElse(null);

        // support unit-test library layout out here (bad design!)
        if (file == null) {
            Arrays.stream(library.getFiles(OrderRootType.CLASSES))
                    .filter(f -> f.getName().equals(namespace + ".r"))
                    .findFirst().orElse(null);
        }

        if (file == null) {
            return;
        }

        final PsiFile psiFile = PsiManager.getInstance(project).findFile(file);

        final RAssignmentStatement[] statements =
                PsiTreeUtil.getChildrenOfType(psiFile, RAssignmentStatement.class);

        if (statements != null) {
            for (RAssignmentStatement statement : statements) {
                final PsiElement assignee = statement.getAssignee();

                if (assignee != null && assignee.getText().equals(name)) {
                    result.add(new PsiElementResolveResult(assignee));
                }
            }
        }
    }


    // just used in ROperatorReference
    public static void resolveWithoutNamespaceInFile(@NotNull final PsiElement element,
                                                     @NotNull final List<ResolveResult> result,
                                                     String... names) {
        for (String name : names) {
            result.addAll(resolveWithoutNamespaceInFile(element, name));
        }
    }


    public static List<ResolveResult> resolveWithoutNamespaceInFile(@NotNull final PsiElement element, String elementName) {
        List<ResolveResult> result = new ArrayList<>();


        // walk up local block hierarchy to resolve symbol in context
        RBlockExpression rBlock = PsiTreeUtil.getParentOfType(element, RBlockExpression.class);
        while (rBlock != null) {
            resolveFromAssignmentInContext(element, elementName, result, rBlock);
            rBlock = PsiTreeUtil.getParentOfType(rBlock, RBlockExpression.class);
        }

        // are we resolving a loop variable?
        // todo unit-test and fix local loops in blocks: { a = 12; for(a in 1:3) print(a); }
        RForStatement rLoop = PsiTreeUtil.getParentOfType(element, RForStatement.class);
        while (rLoop != null) {
            final RExpression target = rLoop.getTarget();
            if (elementName.equals(target.getName())) {
                result.add(new PsiElementResolveResult(target));
            }
            rLoop = PsiTreeUtil.getParentOfType(rLoop, RForStatement.class);
        }

        // are we resolving a symbol in a function expression?
        final RFunctionExpression rFunction = PsiTreeUtil.getParentOfType(element, RFunctionExpression.class);
        if (rFunction != null) {
            final RParameterList list = rFunction.getParameterList();
            for (RParameter parameter : list.getParameterList()) {
                if (elementName.equals(parameter.getName())) {
                    result.add(new PsiElementResolveResult(parameter));
                }
            }
        }

        // more specific lookup was not successful, so search complete file
        final PsiFile file = element.getContainingFile();
        resolveInContextByBlockRecursion(file, element, elementName, result);

//        PsiTreeUtil.getContextOfType(element, RBlockExpression.class, RIfStatement.class, RFile.class)
        //
        PsiElement resolveBarrier = PsiTreeUtil.getContextOfType(element, RFunctionExpression.class);
        if (resolveBarrier != null) {
            resolveInContextByBlockRecursion(resolveBarrier, element, elementName, result);
        }

        // select the most local results (see UnresolvedReferenceInspectionTest.testRedefinedReferenceLookup())
//        if (!result.isEmpty()) result = Lists.newArrayList(Iterables.getLast(result));
        // todo this reduction step should happen later, so that we could access the full set if needed
        if (!result.isEmpty()) {
            Predicate<ResolveResult> fwdRefPredicate = RPsiUtils.createForwardRefPredicate(element);

            ResolveResult bestRef = result.stream().filter(not(fwdRefPredicate)).
                    // get most local backward reference
                            reduce((first, second) -> second).
                    // or first forward reference
                            orElse(Iterables.getFirst(result, null));

            // replace result (since it's final we have to mess around a bit)
            result = Collections.singletonList(bestRef);
//            result.clear();
//            result.add(bestRef);
        }

        return result;
    }


    private static void resolveInContextByBlockRecursion(PsiElement context, @NotNull final PsiElement element, final String elementName, final List<ResolveResult> result) {
        resolveFromAssignmentInContext(element, elementName, result, context);

        // also recurse into blocks, if and elses of current context
        context.acceptChildren(new RVisitor() {
            @Override
            public void visitBlockExpression(@NotNull RBlockExpression o) {
                resolveFromAssignmentInContext(element, elementName, result, o);
                o.acceptChildren(this);
            }


            @Override
            public void visitIfStatement(@NotNull RIfStatement o) {
                super.visitIfStatement(o);
//                resolveFromAssignmentInContext(element, elementName, result, o);

//                PsiTreeUtil.getNextSiblingOfType()
//                PsiTreeUtil.nextVisibleLeaf(element)
//                PsiElement nextSibling = PsiTreeUtil.skipSiblingsBackward(o.getElse(), LeafPsiElement.class);
//                if(nextSibling!=null)
                o.acceptChildren(this);
            }
        });
    }


    public static <T> Predicate<T> not(Predicate<T> t) {
        return t.negate();
    }


    private static void resolveFromAssignmentInContext(PsiElement element, String elementName, @NotNull List<ResolveResult> result, PsiElement context) {
        final RAssignmentStatement[] statements = PsiTreeUtil.getChildrenOfType(context, RAssignmentStatement.class);

        if (statements != null) {
            for (RAssignmentStatement statement : statements) {
                final PsiElement assignee = statement.getAssignee();
                if (assignee != null &&
                        assignee.getText().equals(elementName) &&
                        assignee != element // disallow self-references --> disabled to allow for correct usage search --> see design considerations in devel_notes.md
//                        (assignee.equals(element) )
//                        (assignee.equals(element) || !PsiTreeUtil.isAncestor(statement, element, true))
                        ) {
                    PsiElementResolveResult resolveResult = new PsiElementResolveResult(statement);
                    result.add(resolveResult);
                }
            }
        }
    }


    static void resolveNameArgument(@NotNull final PsiElement element,
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
    static void resolveFunction(PsiElement myElement, String name, List<ResolveResult> result) {
        PsiElement parent = myElement.getParent();

        if (parent != null && parent instanceof RCallExpression) {
            RCallExpression call = ((RCallExpression) parent);
            List<RExpression> arguments = call.getArgumentList().getExpressionList();

            if (call.getExpression().equals(myElement) && !arguments.isEmpty()) {
                RExpression firstArgument = arguments.get(0);
                List<ResolveResult> myResult = new ArrayList<ResolveResult>();

                result.addAll(resolveWithoutNamespaceInFile(myElement, name));
                if (myResult.isEmpty()) {
                    addFromSkeletonsAndRLibrary(myElement, myResult, name);
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

                                        result.addAll(resolveWithoutNamespaceInFile(myElement, genericName));

                                        if (genericResult.isEmpty()) {
                                            addFromSkeletonsAndRLibrary(myElement, genericResult, genericName);
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
