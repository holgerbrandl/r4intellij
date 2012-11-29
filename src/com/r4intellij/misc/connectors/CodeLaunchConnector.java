/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.connectors;

public interface CodeLaunchConnector {


    /**
     * Submit commands to R.
     *
     * @param rCommands     array with commands
     * @param switchFocus2R if <code>true</code>, switch focus console, else does not change the focus.
     * @return <code>false</code>, if not successful, otherwise <code>true</code> (hint)
     */
    void submitCode(String rCommands, boolean switchFocus2R);
}
