

package com.r4intellij.packages;

import java.io.Serializable;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class PckgDataSet implements Serializable {

    private static final long serialVersionUID = -6432397228209255322L;

    private final String name;

    private final String description;


    public PckgDataSet(String name, String description) {
        this.name = name;
        this.description = description;
    }


    public String getName() {
        return name;
    }


    public String getDescription() {
        return description;
    }
}
