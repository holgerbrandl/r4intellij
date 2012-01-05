/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RCr;


public class RCrImpl extends RCompositeElementImpl implements RCr {

    public RCrImpl(ASTNode node) {
        super(node);
    }

}
