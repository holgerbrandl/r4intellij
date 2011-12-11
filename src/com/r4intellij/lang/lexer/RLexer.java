/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang.lexer;

import com.intellij.lexer.FlexAdapter;

import java.io.Reader;


public class RLexer extends FlexAdapter {

    public RLexer() {
        super(new _RLexer((Reader) null));
    }
}
