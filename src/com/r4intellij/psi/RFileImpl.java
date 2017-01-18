package com.r4intellij.psi;

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
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class RFileImpl extends PsiFileBase implements RFile {


    public RFileImpl(FileViewProvider viewProvider) {
        super(viewProvider, RFileType.INSTANCE.getLanguage());
    }


    @Override
    @NotNull
    public FileType getFileType() {
        return RFileType.INSTANCE;
    }


    @Override
    public void accept(@NotNull final PsiElementVisitor visitor) {
        visitor.visitFile(this);
    }


    private CachedValue<List<RCallExpression>> myImports;


    @Override
    public List<RCallExpression> getPckgImportExpressions() {
        if (myImports == null) {
            myImports = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<RCallExpression>>() {
                @Override
                public Result<List<RCallExpression>> compute() {
                    return Result.create(findImportsInFile(), RFileImpl.this);
                }
            }, false);
        }

        return myImports.getValue();
    }


    @Override
    public List<String> getImportedPackages() {
        return getPckgImportExpressions().stream().map(rCall -> {
            List<RExpression> args = rCall.getArgumentList().getExpressionList();

            if (!args.isEmpty())
                return args.get(0).getText();
            else
                return null;

        }).filter(Objects::nonNull).collect(Collectors.toList());
    }


    private List<RCallExpression> findImportsInFile() {
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


    private static boolean isImportStatement(PsiElement psiElement) {
        ArrayList<String> importCommands = Lists.newArrayList("require", "library", "load_pack");

//        ((RCallExpression) psiElement).getArgumentList().getExpressionList().get(0).getText()

        return psiElement instanceof RCallExpression &&
                importCommands.contains(((RCallExpression) psiElement).getExpression().getText());
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
