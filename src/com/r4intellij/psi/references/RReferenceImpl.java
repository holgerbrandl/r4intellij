package com.r4intellij.psi.references;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.module.impl.scopes.LibraryScope;
import com.intellij.openapi.roots.ModifiableModelsProvider;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.RElementGenerator;
import com.r4intellij.RPsiUtils;
import com.r4intellij.interpreter.RInterpreterConfigurable;
import com.r4intellij.interpreter.RSkeletonGenerator;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.stubs.RAssignmentNameIndex;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static com.r4intellij.psi.references.RResolver.*;

public class RReferenceImpl implements PsiPolyVariantReference {
    protected final RReferenceExpression myElement;


    public RReferenceImpl(RReferenceExpression element) {
        myElement = element;
    }


    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        final List<ResolveResult> result = new ArrayList<ResolveResult>();

        if (RPsiUtils.isNamedArgument(myElement)) {
            resolveNameArgument(myElement, myElement.getName(), result);
            return result.toArray(new ResolveResult[result.size()]);
        }

        final String elementName = myElement.getName();
        if (elementName == null) return ResolveResult.EMPTY_ARRAY;

        final String namespace = myElement.getNamespace();
        if (namespace != null) {
            resolveWithNamespace(myElement.getProject(), elementName, namespace, result);
        }

        resolveFunction(myElement, elementName, result);
        if (!result.isEmpty()) {
            return result.toArray(new ResolveResult[result.size()]);
        }

        result.addAll(resolveWithoutNamespaceInFile(myElement, elementName));
        if (!result.isEmpty()) {
            return result.toArray(new ResolveResult[result.size()]);
        }
        addFromSkeletonsAndRLibrary(myElement, result, elementName);
        return result.toArray(new ResolveResult[result.size()]);
    }


    @Override
    public PsiElement getElement() {
        return myElement;
    }


    @Override
    public TextRange getRangeInElement() {
        final TextRange range = myElement.getNode().getTextRange();
        return range.shiftRight(-myElement.getNode().getStartOffset());
    }


    @Nullable
    @Override
    public PsiElement resolve() {
        final ResolveResult[] results = multiResolve(false);
        return results.length >= 1 ? results[0].getElement() : null;
    }


    @NotNull
    @Override
    public String getCanonicalText() {
        return getElement().getText();
    }


    @Override
    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        final ASTNode oldNameIdentifier = getElement().getNode().findChildByType(RElementTypes.R_IDENTIFIER);
        if (oldNameIdentifier != null) {
            final PsiFile dummyFile = RElementGenerator.createDummyFile(newElementName, false, getElement().getProject());
            ASTNode identifier = dummyFile.getNode().getFirstChildNode().findChildByType(RElementTypes.R_IDENTIFIER);
            if (identifier != null) {
                getElement().getNode().replaceChild(oldNameIdentifier, identifier);
            }
        }
        return getElement();
    }


    @Override
    public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
        return null;
    }


    @Override
    public boolean isReferenceTo(PsiElement element) {
        //TODO: check some conditions
        return resolve() == element;
    }


    @NotNull
    @Override
    public Object[] getVariants() {
        List<LookupElement> result = new ArrayList<LookupElement>();
        final String name = myElement.getName();
        if (myElement.getParent() instanceof RReferenceExpression) return ResolveResult.EMPTY_ARRAY;
        if (name == null) return ResolveResult.EMPTY_ARRAY;

        RBlockExpression rBlock = PsiTreeUtil.getParentOfType(myElement, RBlockExpression.class);
        while (rBlock != null) {
            final RAssignmentStatement[] statements = PsiTreeUtil.getChildrenOfType(rBlock, RAssignmentStatement.class);
            if (statements != null) {
                for (RAssignmentStatement statement : statements) {
                    final PsiElement assignee = statement.getAssignee();
                    if (assignee != null) {
                        result.add(LookupElementBuilder.create(assignee.getText()));
                    }
                }
            }
            rBlock = PsiTreeUtil.getParentOfType(rBlock, RBlockExpression.class);
        }
        final RFunctionExpression rFunction = PsiTreeUtil.getParentOfType(myElement, RFunctionExpression.class);
        if (rFunction != null) {
            final RParameterList list = rFunction.getParameterList();
            for (RParameter parameter : list.getParameterList()) {
                result.add(LookupElementBuilder.create(parameter));
            }
        }
        final PsiFile file = myElement.getContainingFile();
        final RAssignmentStatement[] statements = PsiTreeUtil.getChildrenOfType(file, RAssignmentStatement.class);
        if (statements != null) {
            for (RAssignmentStatement statement : statements) {
                final PsiElement assignee = statement.getAssignee();
                if (assignee != null) {
                    result.add(LookupElementBuilder.create(assignee.getText()));
                }
            }
        }
        addVariantsFromSkeletons(result);
        return result.toArray();
    }


    private void addVariantsFromSkeletons(@NotNull final List<LookupElement> result) {
        final ModifiableModelsProvider modelsProvider = ModifiableModelsProvider.SERVICE.getInstance();
        final LibraryTable.ModifiableModel model = modelsProvider.getLibraryTableModifiableModel(myElement.getProject());

        if (model != null) {
            final Library library = model.getLibraryByName(RInterpreterConfigurable.R_SKELETONS);

            final String skeletonsDir = RSkeletonGenerator.getSkeletonsPath();
            if (library != null) {
                final Collection<String> assignmentStatements = RAssignmentNameIndex.allKeys(myElement.getProject());

                for (String statement : assignmentStatements) {
                    final Collection<RAssignmentStatement> statements =
                            RAssignmentNameIndex.find(statement, myElement.getProject(), new LibraryScope(myElement.getProject(), library));

                    for (RAssignmentStatement assignmentStatement : statements) {
                        final PsiDirectory directory = assignmentStatement.getContainingFile().getParent();
                        assert directory != null;

                        if (directory.getName().equals("base") || FileUtil.pathsEqual(directory.getVirtualFile().getCanonicalPath(), skeletonsDir)) {
                            result.add(LookupElementBuilder.create(assignmentStatement));
                        } else {
                            result.add(LookupElementBuilder.create(assignmentStatement, directory.getName() + "::" + assignmentStatement.getName()));
                        }
                    }
                }
            }
        }
    }


    @Override
    public boolean isSoft() {
        return false;
    }
}
