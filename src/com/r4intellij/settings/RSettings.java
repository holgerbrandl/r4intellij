package com.r4intellij.settings;

import com.google.common.base.Strings;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.r4intellij.interpreter.RInterpreterUtil;
import org.jetbrains.annotations.NotNull;

@State(name = "RSettings", storages = @Storage("rSettings.xml"))
public class RSettings implements PersistentStateComponent<RSettings> {

    public String interpreterPath = RInterpreterUtil.suggestHomePath();

    public boolean resolveInModule = false;


    public static RSettings getInstance() {
        return ServiceManager.getService(RSettings.class);
    }


    @Override
    public RSettings getState() {
        //noinspection ReturnOfThis
        return this;
    }


    @Override
    public void loadState(RSettings state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    public String getInterpreterPath() {
//        return interpreterPath;
        // good for testing
//        return null;

        return Strings.emptyToNull(interpreterPath);
    }


    public static boolean hasInterpreter() {
        return RSettings.getInstance().getInterpreterPath() != null;
    }


    public void setInterpreterPath(@NotNull final String interpreterPath) {
        this.interpreterPath = interpreterPath;
    }


    public boolean isResolveInModule() {
        return resolveInModule;
    }


    public void setResolveInModule(boolean resolveInModule) {
        this.resolveInModule = resolveInModule;
    }
}
