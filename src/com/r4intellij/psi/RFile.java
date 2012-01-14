/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.Processor;
import com.r4intellij.RFileTypeLoader;
import com.r4intellij.file.RFileType;
import com.r4intellij.lang.parser.GeneratedParserUtilBase;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;


/**
 * PSI element for an Arc file
 *
 * @author Holger Brandl
 */
public class RFile extends PsiFileBase {

    private CachedValue<List<RProg>> myProgs;

    public RFile(FileViewProvider viewProvider) {
        super(viewProvider, RFileType.R_LANGUAGE);
    }

    @NotNull
    public FileType getFileType() {
        return RFileTypeLoader.R_FILE_TYPE;
    }


    public List<RProg> getRProgs() {
        if (myProgs == null) {
            myProgs = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<RProg>>() {
                @Override
                public Result<List<RProg>> compute() {
                    return Result.create(calcRules(), RFile.this);
                }
            }, false);
        }
        return myProgs.getValue();
    }


    private List<RProg> calcRules() {
        final List<RProg> result = new ArrayList<RProg>();
        processChildrenDummyAware(this, new Processor<PsiElement>() {
            @Override
            public boolean process(PsiElement psiElement) {
                if (psiElement instanceof RProg) {
                    result.add((RProg) psiElement);
                }
                return true;
            }
        });
        return result;
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
