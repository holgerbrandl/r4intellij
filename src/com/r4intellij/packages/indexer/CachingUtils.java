/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages.indexer;

import com.r4intellij.interpreter.RInterpreterService;

import java.io.*;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
@Deprecated
public class CachingUtils {


    public static String evalRScript(File script) {
        String[] cmd = new String[]{RInterpreterService.getInstance().getInterpreterPath(), "--vanilla", "--quiet", "-f", script.getAbsolutePath()};
        try {
            return evalRInternal(cmd).getOutput();
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }


    static String evalRCommand(String cmd) {
        return evalRCmd(cmd).getOutput();
    }


    static String evalRCommandCat(String cmd) {
        return evalRCmd("cat(" + cmd + ", sep='\\\\n')").getOutput().trim();
//        return evalRCmd("cat("+cmd+")").getOutput().trim();
    }


    static StreamGobbler evalRCmd(String cmd) {
//        cmd = Utils.isWindowsPlatform() ? cmd.replaceAll("[$]", "\\$") : cmd;
        String[] getPckgsCmd = new String[]{RInterpreterService.getInstance().getInterpreterPath(), "--vanilla", "--quiet", "--slave", "-e", cmd};

        return evalRInternal(getPckgsCmd);
    }


    private static StreamGobbler evalRInternal(String[] getPckgsCmd) {

        try {
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
        } catch (Throwable t) {
            throw new RuntimeException(t);
        }
    }


    static Object loadObject(File f) {
        try {
            FileInputStream fin = new FileInputStream(f);
            ObjectInputStream ois = new ObjectInputStream(fin);
            Object o = ois.readObject();
            ois.close();

            return o;
        } catch (Throwable e) {
            System.err.println("could not load R package index");
        }

        return null;
    }


    static void saveObject(Object o, File f) {
        try {
            FileOutputStream fout = new FileOutputStream(f);
            ObjectOutputStream oos = new ObjectOutputStream(fout);
            oos.writeObject(o);
            oos.close();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }


    public static void main(String[] args) {

//        System.err.println((evalRCmd("iris$Sepal.Length").getOutput()));
        System.err.println((evalRCmd("tt <-library(help=base); tt$info[[1]]").getOutput()));
        System.err.println(LibraryIndexFactory.getPackageVersion("base"));
    }
}


@Deprecated
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
