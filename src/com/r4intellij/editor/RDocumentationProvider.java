/*
 * Copyright 2011-2011 Gregory Shrago
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.r4intellij.editor;

import com.intellij.lang.documentation.DocumentationProvider;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiManager;
import com.r4intellij.misc.rinstallcache.Function;
import com.r4intellij.misc.rinstallcache.PackageCache;
import com.r4intellij.misc.rinstallcache.PackageCacheService;
import com.r4intellij.misc.rinstallcache.RCacheUtils;
import com.r4intellij.psi.RFile;
import com.r4intellij.psi.RFuncall;
import com.r4intellij.psi.RVariable;
import org.jetbrains.annotations.Nullable;

import java.util.List;


/**
 * @author Holger Brandl
 */
public class RDocumentationProvider implements DocumentationProvider {

    @Nullable
    public String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        element = element instanceof RVariable && element.getParent() instanceof RFuncall ? element.getParent() : element;

        if (element instanceof RFuncall) {
            Function function = getFunction(((RFuncall) element).getVariable());
            return "<h1>" + function.getFunName() + "</h1>" + function.getFunDesc() + "<br><i>" + function.getBasicFunSignature() + "</i>";
        }
        return null;
    }

    private Function getFunction(PsiElement element) {
        PackageCacheService cacheService = ServiceManager.getService(PackageCacheService.class);
        PackageCache cache = cacheService.getCache();
        if (cache == null)
            return null;


        RFile file = (RFile) element.getContainingFile();

        List<Function> functions = RCacheUtils.getFunctionByName(element.getText(), RCacheUtils.getImportedPackages(file));


        if (functions.isEmpty()) // seach the complete index if the function is not yet imported
            functions = RCacheUtils.getFunctionByName(element.getText(), null);

        if (functions.isEmpty())
            return null;

        // just take the first one for now
        return functions.get(0);
    }

    @Nullable
    public List<String> getUrlFor(final PsiElement element, final PsiElement originalElement) {
        return null;
    }

    @Nullable
    public String generateDoc(final PsiElement element, final PsiElement originalElement) {
        return getQuickNavigateInfo(element, originalElement);

//        if (element instanceof BnfRule) {
//            BnfRule rule = (BnfRule) element;
//            Set<String> first = BnfFirstNextAnalyzer.calcFirst(rule);
//            Set<String> next = BnfFirstNextAnalyzer.calcNext(rule);
//
//            boolean hasNull = first.remove(BnfFirstNextAnalyzer.EMPTY_STRING);
//            if (hasNull) first.add("<eof>");
//            boolean hasNull2 = next.remove(BnfFirstNextAnalyzer.EMPTY_STRING);
//            if (hasNull2) next.add("<eof>");
//            String[] firstS = first.toArray(new String[first.size()]);
//            Arrays.sort(firstS);
//            String[] nextS = next.toArray(new String[next.size()]);
//            Arrays.sort(nextS);
//            return "<h1>Starts with:</h1>" + StringUtil.escapeXml(StringUtil.join(firstS, " | "))
//                    + "<br><h1>Followed by:</h1>" + StringUtil.escapeXml(StringUtil.join(nextS, " | "));
//        }
//        return null;
    }

    @Nullable
    public PsiElement getDocumentationElementForLookupItem(final PsiManager psiManager, final Object object, final PsiElement element) {
        return null;
    }

    @Nullable
    public PsiElement getDocumentationElementForLink(final PsiManager psiManager, final String link, final PsiElement context) {
        return null;
    }
}
