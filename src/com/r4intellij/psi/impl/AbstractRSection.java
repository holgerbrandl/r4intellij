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
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;


/**
 * @author Holger Brandl
 */
public abstract class AbstractRSection extends RCompositeElementImpl {

    public AbstractRSection(ASTNode node) {
        super(node);
    }

    @Override
    public String getName() {
        return getNameInternal();
    }

    private String getNameInternal() {
        String text = getText();
        if (text == null) {
            text = "txt was nuill";
        }
        String sectionName = text.replace("#", "").trim();
        if (sectionName.isEmpty()) {
            sectionName = "Unnamed section";
        }
        return sectionName;
    }
}
