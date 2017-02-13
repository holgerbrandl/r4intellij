package com.r4intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.r4intellij.interpreter.RInterpreterUtil;
import org.jetbrains.annotations.NotNull;

// todo name of component looks wrong
@State(
        name = "RSettings",
        storages = {@Storage(file = "rSettings.xml")}
)
public class RSettings implements PersistentStateComponent<RSettings> {

    public String interpreterPath = RInterpreterUtil.suggestHomePath();
    public String INTERPRETER_SOURCES_PATH = "";

    public boolean resolveWithinModule = false;


    public static RSettings getInstance() {
        return ServiceManager.getService(RSettings.class);
    }


    @Override
    public RSettings getState() {
        return this;
    }


    @Override
    public void loadState(RSettings state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    public String getInterpreterPath() {
        return interpreterPath;
    }


    public void setInterpreterPath(@NotNull final String interpreterPath) {
        this.interpreterPath = interpreterPath;
    }


    public boolean isResolveWithinModule() {
        return resolveWithinModule;
    }


    public void setResolveWithinModule(boolean resolveWithinModule) {
        this.resolveWithinModule = resolveWithinModule;
    }


    public String getSourcesPath() {
        return INTERPRETER_SOURCES_PATH;
    }


    public void setSourcesPath(@NotNull final String interpreterSourcesPath) {
        INTERPRETER_SOURCES_PATH = interpreterSourcesPath;
    }
}
