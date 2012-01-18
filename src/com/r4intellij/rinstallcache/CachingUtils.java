/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.rinstallcache;

import java.io.*;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class CachingUtils {


    public static String evalRScript(File script) {
        String[] cmd = new String[]{"R", "--vanilla", "--quiet", "-f", script.getAbsolutePath()};
        try {
            return evalRInternal(cmd).getOutput();
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    static String evalRComand(String cmd) throws IOException, InterruptedException {
        return evalRCmd(cmd).getOutput();
    }


    static StreamGobbler evalRCmd(String cmd) throws IOException, InterruptedException {
        String[] getPckgsCmd = new String[]{"R", "--vanilla", "--quiet", "-e", cmd};

        return evalRInternal(getPckgsCmd);
    }

    private static StreamGobbler evalRInternal(String[] getPckgsCmd) throws IOException, InterruptedException {

//        String osName = System.getProperty("os.name" );
//        String[] cmd = new String[3];
//        if( osName.equals( "Windows NT" ) )
//        {
//            cmd[0] = "cmd.exe" ;
//            cmd[1] = "/C" ;
//            cmd[2] = args[0];
//        }


        Process proc = Runtime.getRuntime().exec(getPckgsCmd);

        StreamGobbler errorGobbler = new
                StreamGobbler(proc.getErrorStream(), "ERROR");

        // any output?
        StreamGobbler outputGobbler = new
                StreamGobbler(proc.getInputStream(), "OUTPUT");

        // kick them off
        errorGobbler.start();
        outputGobbler.start();

        // any error???
        int exitVal = proc.waitFor();
        return outputGobbler;
    }

}


class StreamGobbler extends Thread {

    InputStream is;
    String type;
    StringBuilder sb = new StringBuilder();

    StreamGobbler(InputStream is, String type) {
        this.is = is;
        this.type = type;
    }

    public void run() {
        try {
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line = null;
            while ((line = br.readLine()) != null) {
                sb.append(line + "\n");
//                System.out.println(type + ">" + line);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace();
        }
    }

    public String getOutput() {
        return sb.toString();
    }
}
