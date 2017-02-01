package com.r4intellij.interpreter;

import com.intellij.openapi.components.*;
import com.intellij.util.xmlb.XmlSerializerUtil;
import org.jetbrains.annotations.NotNull;

@State(
        name = "RInterpreterService",
        storages = {
                @Storage(
                        file = StoragePathMacros.APP_CONFIG + "/rInterpreterSettings.xml"
                )}
)
public class RInterpreterService implements PersistentStateComponent<RInterpreterService> {

    public String INTERPRETER_PATH = RInterpreterUtil.suggestHomePath();
    public String INTERPRETER_SOURCES_PATH = "";


    public static RInterpreterService getInstance() {
        return ServiceManager.getService(RInterpreterService.class);
    }


    @Override
    public RInterpreterService getState() {
        return this;
    }


    @Override
    public void loadState(RInterpreterService state) {
        XmlSerializerUtil.copyBean(state, this);
    }


    public String getInterpreterPath() {
        return INTERPRETER_PATH;
    }


    public void setInterpreterPath(@NotNull final String interpreterPath) {
        INTERPRETER_PATH = interpreterPath;
    }


    public String getSourcesPath() {
        return INTERPRETER_SOURCES_PATH;
    }


    public void setSourcesPath(@NotNull final String interpreterSourcesPath) {
        INTERPRETER_SOURCES_PATH = interpreterSourcesPath;
    }
}
