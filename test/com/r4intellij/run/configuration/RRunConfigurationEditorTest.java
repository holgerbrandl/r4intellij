package com.r4intellij.run.configuration;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.util.containers.HashMap;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

public class RRunConfigurationEditorTest {

    @NotNull
    private static final Project PROJECT = mock(Project.class);

    @NotNull
    private static final ConfigurationFactory CONFIGURATION_FACTORY = mock(ConfigurationFactory.class);


    @Test
    public void resetApply() throws ConfigurationException {
        final RRunConfiguration runConfiguration1 = createConfiguration("s_p_1", "s_a_1", "w_d_p_1", "k1", true);
        final RRunConfiguration runConfiguration2 = createConfiguration("s_p_2", "s_a_2", "w_d_p_2", "k2", false);

        final RRunConfigurationEditor editor = new RRunConfigurationEditor(PROJECT);
        editor.resetEditorFrom(runConfiguration1);
        editor.applyEditorTo(runConfiguration2);

        assertParamsEquals(runConfiguration1, runConfiguration2);
    }


    @NotNull
    private RRunConfiguration createConfiguration(@NotNull final String scriptPath,
                                                  @NotNull final String scriptArgs,
                                                  @NotNull final String workingDirectoryPath,
                                                  @NotNull final String envKey,
                                                  final boolean passParentEnvs) {
        final RRunConfiguration result = new RRunConfiguration(PROJECT, CONFIGURATION_FACTORY);

        final Map<String, String> envs = new HashMap<String, String>();
        envs.put(envKey, "v");

        result.setScriptPath(scriptPath);
        result.setScriptArgs(scriptArgs);
        result.setWorkingDirectoryPath(workingDirectoryPath);
        result.setEnvs(envs);
        result.setPassParentEnvs(passParentEnvs);

        return result;
    }


    private void assertParamsEquals(@NotNull final RRunConfigurationParams params1, @NotNull final RRunConfigurationParams params2) {
        assertEquals(params1.getScriptPath(), params2.getScriptPath());
        assertEquals(params1.getScriptArgs(), params2.getScriptArgs());
        assertEquals(params1.getWorkingDirectoryPath(), params2.getWorkingDirectoryPath());
        assertEquals(params1.isPassParentEnvs(), params2.isPassParentEnvs());
        assertEquals(params1.getEnvs(), params2.getEnvs());
    }
}