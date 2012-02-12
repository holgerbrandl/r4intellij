/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.rinstallcache;

import com.r4intellij.Utils;

import java.io.*;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class CachingUtils {


    public static String evalRScript(File script) {
        String[] cmd = new String[]{getRExecutable(), "--vanilla", "--quiet", "-f", script.getAbsolutePath()};
        try {
            return evalRInternal(cmd).getOutput();
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }

    private static String getRExecutable() {
        if (!Utils.isWindowsPlatform())
            return "R";

        File rHome = new File(System.getenv("R_HOME"));
        if (rHome == null) {
            rHome = new File(System.getenv("ProgramFiles") + File.separatorChar + "R");
        }
        if (rHome.isDirectory()) {
            List<File> rInstallations = Arrays.asList(rHome.listFiles(new FilenameFilter() {
                @Override
                public boolean accept(File dir, String name) {
                    Matcher matcher = Pattern.compile("R-([0-9.]*)").matcher(name);
                    matcher.find();
                    return matcher.matches();
                }
            }));

            File latestR = Collections.max(rInstallations, new Comparator<File>() {
                @Override
                public int compare(File o1, File o2) {
                    int v1 = Integer.parseInt(o1.getName().replace("R-", "").replaceAll("[.]", ""));
                    int v2 = Integer.parseInt(o2.getName().replace("R-", "").replaceAll("[.]", ""));

                    return v1 - v2;  //To change body of implemented methods use File | Settings | File Templates.
                }
            });

            return latestR.getAbsolutePath() + File.separatorChar + "bin" + File.separatorChar + "R.exe";
        }

        return null;
    }


    static String evalRComand(String cmd) {
        return evalRCmd(cmd).getOutput();
    }


    static StreamGobbler evalRCmd(String cmd) {
//        cmd = Utils.isWindowsPlatform() ? cmd.replaceAll("[$]", "\\$") : cmd;
        String[] getPckgsCmd = new String[]{getRExecutable(), "--vanilla", "--quiet", "-e", cmd};

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
        System.err.println(LibraryIndexFactory.getPackageVersions(Arrays.asList("base")));
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
