/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.psi;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.Processor;
import com.r4intellij.lang.RFileType;
import com.r4intellij.lang.RFileTypeLoader;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;


/**
 * PSI element for an Arc file
 *
 * @author Holger Brandl
 */
public class RFile extends PsiFileBase {

    private CachedValue<List<RCommand>> myProgs;
    private CachedValue<List<RFuncall>> myImports;


    public RFile(FileViewProvider viewProvider) {
        super(viewProvider, RFileType.R_LANGUAGE);
    }


    @NotNull
    public FileType getFileType() {
        return RFileTypeLoader.R_FILE_TYPE;
    }


    public List<RCommand> getRProgs() {
        if (myProgs == null) {
            myProgs = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<RCommand>>() {
                @Override
                public Result<List<RCommand>> compute() {
                    return Result.create(calcCommands(), RFile.this);
                }
            }, false);
        }
        return myProgs.getValue();
    }


    private List<RCommand> calcCommands() {
        final List<RCommand> result = new ArrayList<RCommand>();
        processChildrenDummyAware(this, new Processor<PsiElement>() {
            @Override
            public boolean process(PsiElement psiElement) {
                if (psiElement instanceof RCommand) {
                    result.add((RCommand) psiElement);
                }
                return true;
            }
        });
        return result;
    }


    public List<RFuncall> getImportStatements() {
        if (myImports == null) {
            myImports = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<RFuncall>>() {
                @Override
                public Result<List<RFuncall>> compute() {
                    return Result.create(calcImports(), RFile.this);
                }
            }, false);
        }
        return myImports.getValue();
    }


    public List<RSection> calcSections() {
        final List<RSection> result = new ArrayList<RSection>();
        processChildrenDummyAware(this, new Processor<PsiElement>() {
            @Override
            public boolean process(PsiElement psiElement) {
                if (psiElement instanceof RSection) {
                    result.add((RSection) psiElement);
                }
//                this.process(psiElement.getChildren());
                processChildrenDummyAware(psiElement, this);

                return true;
            }
        });

        //sort the sections
        Collections.sort(result, new Comparator<RSection>() {
            @Override
            public int compare(RSection rSection, RSection rSection1) {
                return rSection.getTextOffset() < rSection1.getTextOffset() ? -1 : 1;
            }
        });

        return result;
    }


    private List<RFuncall> calcImports() {
        final List<RFuncall> result = new ArrayList<RFuncall>();
        processChildrenDummyAware(this, new Processor<PsiElement>() {
            @Override
            public boolean process(PsiElement psiElement) {
                if (psiElement instanceof RFuncall && ((RFuncall) psiElement).getVariable().getText().equals("library")) {
                    result.add((RFuncall) psiElement);
                }
//                this.process(psiElement.getChildren());
                processChildrenDummyAware(psiElement, this);

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
