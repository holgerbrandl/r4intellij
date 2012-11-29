/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RSection;


public class RSectionImpl extends AbstractRSection implements RSection {

    public RSectionImpl(ASTNode node) {
        super(node);
    }

}
