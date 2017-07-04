

package com.r4intellij.packages;

import java.io.Serializable;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class PckgDataSet implements Serializable {

    private static final long serialVersionUID = -6432397228209255332L;

    private String name;


    public PckgDataSet(String name) {
        this.name = name;
    }


    public String getName() {
        return name;
    }
}
