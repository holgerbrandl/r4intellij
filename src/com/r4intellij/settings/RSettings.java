package com.r4intellij.settings;

import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.ServiceManager;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.r4intellij.interpreter.RInterpreterUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;

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


    @Nullable
    public String getInterpreterPath() {
        // disabled because cyclic loop
//        if(!hasInterpreter()) return null;


        return interpreterPath;
    }


    public static boolean hasInterpreter() {
        String interpreterPath = RSettings.getInstance().getInterpreterPath();
        return !StringUtil.isEmptyOrSpaces(interpreterPath) && new File(interpreterPath).canExecute();
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
