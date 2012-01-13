/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.connectors;

import com.r4intellij.Utils;

import java.io.IOException;


/**
 * A connector using apple script
 *
 * @author Holger Brandl
 */
public class AppleScriptConnector implements CodeLaunchConnector {

    @Override
    public void submitCode(String rCommands, boolean switchFocus2R) {
        try {
            if (Utils.isMacOSX()) {
                Runtime runtime = Runtime.getRuntime();

                String dquotesExpandedText = rCommands.replace("\"", "\\\"");
                String evalSelection = "tell application \"R64\" to activate\n" +
                        "tell application \"R64\" to cmd \"" + dquotesExpandedText + "\"";


                String[] args = {"osascript", "-e", evalSelection};

                runtime.exec(args);
            }
        } catch (IOException e1) {
            ConnectorUtils.log.error(e1);
        }
    }
}
