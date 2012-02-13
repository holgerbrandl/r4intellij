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

import com.r4intellij.Utils;

import java.io.*;
import java.lang.reflect.InvocationTargetException;


public class RGWLauncher implements CodeLaunchConnector {


    private boolean fSubmitDirectly;
    private String fExecutable;

    public static void main(String[] args) {
//        new RGWLauncher().submitCode("print('test')", true);
//        System.err.println(new RGWLauncher().getLauncher());
        new RGWLauncher().submitCode("ls()", false);

        System.err.println();
    }

    public RGWLauncher() {

        // todo
//        final URL dir = WinRGuiConnectorPlugin.getDefault().getBundle().getEntry("/win32/RGWConnector.exe"); //$NON-NLS-1$
        try {
//            final String local = FileLocator.toFileURL(dir).getPath();
            fExecutable = getLauncher();
        } catch (final Throwable e) {
            throw new RuntimeException("Error Loading R-GUI-Windows-Connector:", e);
        }
    }

    private static String getLauncher() {
        File rgwExe = new File(System.getProperty("user.home") + File.separator + "RGWConnector.exe");
        if (!rgwExe.isFile()) {
            copyStream2File(rgwExe);
        }

        if (!rgwExe.exists())
            throw new RuntimeException("Missing connector exe  '" + rgwExe.getAbsolutePath() + "'.");
        return rgwExe.getAbsolutePath();
    }

    private static void copyStream2File(File rgwExe) {
        try {
//            InputStream in = Thread.currentThread().getContextClassLoader().getResourceAsStream("/connectors/RGWConnector.exe");
            InputStream in = Utils.class.getResourceAsStream("/connectors/RGWConnector.exe");
            OutputStream out = new BufferedOutputStream(new FileOutputStream(rgwExe));
            byte[] buffer = new byte[1024];
            int len;
            while ((len = in.read(buffer)) != -1) {
                out.write(buffer, 0, len);
            }

            out.flush();
            out.close();
            in.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
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
