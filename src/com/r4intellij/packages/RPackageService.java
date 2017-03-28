package com.r4intellij.packages;

import com.google.common.collect.Lists;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author avesloguzova
 * @author holgerbrandl
 */

// i.e. ~/Library//Caches/IntelliJIdea2017.1/plugins-sandbox/config/options/rpackages.xml
//        /Users/brandl/Library/Preferences/IntelliJIdea2016.3/options/rpackages.xml

@SuppressWarnings("WeakerAccess")
@State(name = "RPackageService", storages = {@Storage(file = "rpackages.xml")})
public class RPackageService implements PersistentStateComponent<RPackageService> {

    public static final Logger LOG = Logger.getInstance("#" + RPackageService.class.getName());


    public int CRANMirror = 1;

    public List<String> enabledRepositories = Lists.newArrayList();

    public List<String> userRepositories = Lists.newArrayList();


    @Nullable
    @Override
    public RPackageService getState() {
        return this;
    }


    @Override
    public void loadState(RPackageService state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    @Override
    public void noStateLoaded() {
        System.err.println("");
    }
//todo synchronized is ugly here, but otherwise getInstance is not yet done when after its first invocation
    // and retriggers an index refresh


    public synchronized static RPackageService getInstance() {
        return ServiceManager.getService(RPackageService.class);
    }

}
