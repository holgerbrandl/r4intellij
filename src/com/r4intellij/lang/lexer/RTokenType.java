/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang.lexer;

import com.intellij.psi.tree.IElementType;
import com.r4intellij.lang.RFileType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;


/**
 * The definition of a R element type.
 *
 * @author Holger Brandl
 */
public class RTokenType extends IElementType {

    public RTokenType(@NotNull @NonNls String debugName) {
        super(debugName, RFileType.R_LANGUAGE);
    }

	@Override
	public String toString() {
		return ".R_" + super.toString();
	}
}
