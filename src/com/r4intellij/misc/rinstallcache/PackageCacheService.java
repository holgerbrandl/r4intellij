/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.rinstallcache;

import com.intellij.openapi.diagnostic.Logger;


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

            }
        }.start();
        logger.info("loading of r-package cache done");
    }

    public PackageCache getCache() {
        return pcache;
    }
}
