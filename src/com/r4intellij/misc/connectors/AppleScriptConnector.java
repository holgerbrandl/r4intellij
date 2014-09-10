/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.connectors;

import com.r4intellij.Utils;
import com.r4intellij.settings.RSettings;

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

                String dquotesExpandedText = rCommands.replace("\\", "\\\\");
                dquotesExpandedText = dquotesExpandedText.replace("\"", "\\\"");

				String[] lines = dquotesExpandedText.split("\n");

				for(String line : lines)
				{
//					if(line.trim().startsWith("#"))
//						continue;

					String evalTarget = RSettings.getInstance().codeSnippetEvalTarget;
//                String evalTarget = "R64";

//                //todo remove this hacky thing
//                File connectorDef = new File(System.getProperty("user.home") + File.separator + "r4j_evaltarget.txt");
//                if(connectorDef.exists()) {
//                    evalTarget = Utils.readFileAsString(connectorDef.getAbsolutePath()).replace("\n", "");
//                }

//                http://stackoverflow.com/questions/1870270/sending-commands-and-strings-to-terminal-app-with-applescript

					String evalSelection;
					if (evalTarget.equals("Terminal")) {
						evalSelection = "tell application \"" + "Terminal" + "\" to do script \"" + line + "\" in window 0";

						if (switchFocus2R) {
							evalSelection = "tell application \"Terminal\" to activate\n" + evalSelection;
						}

					} else if (evalTarget.equals("iTerm")) {
						evalSelection = "tell application \"iTerm\" to tell current session of current terminal  to write text  \"" + line + "\"";
						if (switchFocus2R) {
							evalSelection = "tell application \"iTerm\" to activate\n" + evalSelection;
						}

					} else {
						if (switchFocus2R) {
							evalSelection = "tell application \"" + evalTarget + "\" to activate\n" +
									"tell application \"" + evalTarget + "\" to cmd \"" + dquotesExpandedText + "\"";
						} else {
							evalSelection = "tell application \"" + evalTarget + "\" to cmd \"" + dquotesExpandedText + "\"";
						}
					}

					String[] args = {"osascript", "-e", evalSelection};

					runtime.exec(args);
				}
            }
        } catch (IOException e1) {
            ConnectorUtils.log.error(e1);
        }
    }


    public static void main(String[] args) {
        new AppleScriptConnector().submitCode("write.table(head(iris), file=\"~/Desktop/iris.txt\", sep=\"\\t\")\n", true);
    }
}
