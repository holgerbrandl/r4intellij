/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.rinstallcache;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzer;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class PackageCacheService {

    public static Logger logger = Logger.getInstance("RPackageCache");

    private PackageCache pcache;


    public PackageCacheService() {
        logger.info("loading r-package cache");

        new Thread() {

            @Override
            public void run() {
                super.run();    //To change body of overridden methods use File | Settings | File Templates.

                pcache = PackageCache.getLibraryCache();

                restartInspections();
            }
        }.start();
        logger.info("loading of r-package cache done");
    }


    public static void restartInspections() {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            public void run() {
                Project[] projects = ProjectManager.getInstance().getOpenProjects();
                for (Project project : projects) {
                    if (project.isInitialized() && project.isOpen() && !project.isDefault()) {
                        DaemonCodeAnalyzer.getInstance(project).restart();
                    }
                }
            }
        });
    }


    public PackageCache getCache() {
        return pcache;
    }
}
