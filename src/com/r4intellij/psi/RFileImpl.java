package com.r4intellij.psi;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.Processor;
import com.r4intellij.RFileType;
import com.r4intellij.RPsiUtils;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.r4intellij.editor.RCompletionContributor.PACKAGE_IMPORT_METHODS;
import static com.r4intellij.packages.RSkeletonGenerator.DEFAULT_PACKAGES;

public class RFileImpl extends PsiFileBase implements RFile {


    public RFileImpl(FileViewProvider viewProvider) {
        super(viewProvider, RFileType.INSTANCE.getLanguage());
    }


    @Override
    @NotNull
    public FileType getFileType() {
        return RFileType.INSTANCE;
    }


    // just needed to for compatibility with exsting test-results. Should be removed and results should be updated
    @Override
    public String toString() {
        return "RFile:" + getName();
    }


    @Override
    public void accept(@NotNull final PsiElementVisitor visitor) {
        visitor.visitFile(this);
    }


    private CachedValue<List<RCallExpression>> myImports;


    @Override
    public List<RCallExpression> getImportExpressions(PsiElement queryElement) {
        if (myImports == null) {
            myImports = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<RCallExpression>>() {
                @Override
                public Result<List<RCallExpression>> compute() {
                    return Result.create(findImports(), RFileImpl.this);
                }
            }, false);
        }

        List<RCallExpression> allImports = myImports.getValue();

        Predicate<RCallExpression> beforeQueryPredicate = is -> RPsiUtils.isForwardReference(queryElement, is);
        return allImports.stream().filter(beforeQueryPredicate).collect(Collectors.toList());
    }


    private List<RCallExpression> findImports() {

        final List<RCallExpression> result = new ArrayList<RCallExpression>();
        processChildrenDummyAware(this, new Processor<PsiElement>() {
            @Override
            public boolean process(PsiElement psiElement) {
                if (isImportStatement(psiElement)) {
                    result.add((RCallExpression) psiElement);
                }
//                this.process(psiElement.getChildren());
                processChildrenDummyAware(psiElement, this);

                return true;
            }
        });
        return result;
    }


    @Override
    public List<String> getImportedPackages(PsiElement element) {
        // use cache to get list of imported package names
        List<String> imports = getImportExpressions(element).stream().map(rCall -> {
            List<RExpression> args = rCall.getArgumentList().getExpressionList();

            if (!args.isEmpty())
                return args.get(0).getText();
            else
                return null;

        }).filter(Objects::nonNull).collect(Collectors.toList());

        // prefix with default libraries
        return Lists.newArrayList(Iterables.concat(DEFAULT_PACKAGES, imports));
    }




    private static boolean isImportStatement(PsiElement psiElement) {
        ;

//        ((RCallExpression) psiElement).getArgumentList().getExpressionList().get(0).getText()

        return psiElement instanceof RCallExpression &&
                PACKAGE_IMPORT_METHODS.contains(((RCallExpression) psiElement).getExpression().getText());
    }


    private static boolean processChildrenDummyAware(PsiElement element, final Processor<PsiElement> processor) {
        return new Processor<PsiElement>() {
            @Override
            public boolean process(PsiElement psiElement) {
                for (PsiElement child = psiElement.getFirstChild(); child != null; child = child.getNextSibling()) {
                    if (child instanceof GeneratedParserUtilBase.DummyBlock) {
                        if (!process(child)) return false;
                    } else if (!processor.process(child)) return false;
                }
                return true;
            }
        }.process(element);
    }
}
