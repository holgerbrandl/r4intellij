/*******************************************************************************
 * Copyright (c) 2005-2011 WalWare/StatET-Project (www.walware.de/goto/statet).
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Stephan Wahlbrink - initial API and implementation
 *******************************************************************************/

package com.r4intellij.misc.connectors;

import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;


public class RGWLauncher implements CodeLaunchConnector {


    private boolean fSubmitDirectly;
    private String fExecutable;

    public static void main(String[] args) {
        new RGWLauncher().submitCode("print('test')", true);
    }

    public RGWLauncher() {

        // todo
//        final URL dir = WinRGuiConnectorPlugin.getDefault().getBundle().getEntry("/win32/RGWConnector.exe"); //$NON-NLS-1$
        try {
//            final String local = FileLocator.toFileURL(dir).getPath();
            getLauncher();
        } catch (final IOException e) {
            throw new RuntimeException("Error Loading R-GUI-Windows-Connector:", e);
        }
    }

    private static String getLauncher() throws IOException {
        // todo fixme
        final File file = new File("D:\\RGWConnector.exe");
        if (!file.exists())
            throw new IOException("Missing File '" + file.getAbsolutePath() + "'.");
        return file.getAbsolutePath();
    }

    private enum SubmitType {DONOTHING, SUBMITINPUT, PASTECLIPBOARD}


    @Override
    public void submitCode(final String rCommands, boolean switchFocus2R) {
        final String[] processCmd = new String[]{fExecutable, SubmitType.SUBMITINPUT.toString().toLowerCase()};

        // todo use injtellij process status here :  UsageViewImplUtil.runProcessWithProgress
//		final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();
//
//		final IRunnableWithProgress runnable = new IRunnableWithProgress(){
//			public void run(final IProgressMonitor monitor) throws InvocationTargetException {
        try {
            Process process = Runtime.getRuntime().exec(processCmd);
            writeTextToProcess(process, new String[]{rCommands}); // todo maybe we have to split it into lines
            process.waitFor();

        } catch (final Exception e) {
            throw new RuntimeException(new InvocationTargetException(e));
        }
    }


    private void writeTextToProcess(final Process process, final String[] text) {
        PrintWriter writer = null;
        try {
            writer = new PrintWriter(new OutputStreamWriter(process.getOutputStream()));
            for (int i = 0; i < text.length; i++) {
                writer.println(text[i]);
            }
        } catch (final Exception ignored) {

        } finally {
            if (writer != null)
                try {
                    writer.close();
                } catch (final Exception ignored) {
                }
        }
    }

    private boolean copyToClipboard(final String[] text) {
//		final StringBuilder builder = new StringBuilder();
//		for (int i = 0; i < text.length; i++) {
//			builder.append(text[i]);
//			builder.append("\n");
//		}
//		
//		if (fClipboard == null) {
//			fClipboard = new Clipboard(Display.getCurrent());
//		}
//		
//		return DNDUtil.setContent(fClipboard,
//				new String[] { builder.toString() },
//				new Transfer[] { TextTransfer.getInstance() } );
        return false;
    }

}
