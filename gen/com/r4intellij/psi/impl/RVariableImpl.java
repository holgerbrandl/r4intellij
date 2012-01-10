/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RVariable;


public class RVariableImpl extends RNamedElementImpl implements RVariable {

    public RVariableImpl(ASTNode node) {
        super(node);
    }

}
