/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij;

import com.intellij.lang.Language;


/**
 * The definition of the R language for our plugin.
 *
 * @author Holger Brandl
 */
public class RLanguage extends Language {

    public static final RLanguage INSTANCE = new RLanguage();

    public RLanguage() {
        super("R", "text/R");
    }
}
