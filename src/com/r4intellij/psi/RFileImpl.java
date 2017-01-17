package com.r4intellij.psi;

import com.google.common.base.Function;
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
import com.r4intellij.psi.api.RFile;
import com.r4intellij.psi.api.RFunctionExpression;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

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


    private CachedValue<List<RFunctionExpression>> myImports;


    @Override
    public List<RFunctionExpression> getPckgImportExpressions() {
        if (myImports == null) {
            myImports = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<RFunctionExpression>>() {
                @Override
                public Result<List<RFunctionExpression>> compute() {
                    return Result.create(calcImports(), RFileImpl.this);
                }
            }, false);
        }

        return myImports.getValue();
    }


    @Override
    public List<String> getImportedPackages() {
        return Lists.newArrayList(Iterables.transform(getPckgImportExpressions(),
                new Function<RFunctionExpression, String>() {

                    @Override
                    public String apply(RFunctionExpression rFunctionExpression) {
                        return rFunctionExpression.getReference().getCanonicalText();
                    }
                }));
    }


    private List<RFunctionExpression> calcImports() {
        final List<RFunctionExpression> result = new ArrayList<RFunctionExpression>();
        processChildrenDummyAware(this, new Processor<PsiElement>() {
            @Override
            public boolean process(PsiElement psiElement) {
                if (isImportStatement(psiElement)) {
                    result.add((RFunctionExpression) psiElement);
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

        return psiElement instanceof RFunctionExpression &&
                importCommands.contains(((RFunctionExpression) psiElement).getExpression().getText());
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
