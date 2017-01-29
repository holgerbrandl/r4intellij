package com.r4intellij.run.configuration;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.openapi.project.Project;
import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

public class RRunConfigurationTypeTest {

    @Test
    public void idNameDescription() {
        final RRunConfigurationType configurationType = new RRunConfigurationType();

        assertEquals("RRunConfigurationType", configurationType.getId());
        assertEquals("R", configurationType.getDisplayName());
        assertEquals("R run configuration", configurationType.getConfigurationTypeDescription());
    }


    @Test
    public void template() {
        final Project project = mock(Project.class);

        final ConfigurationFactory configurationFactory = new RRunConfigurationType().getMainFactory();
        final RRunConfiguration templateConfiguration = (RRunConfiguration) configurationFactory.createTemplateConfiguration(project);

        assertEquals("", templateConfiguration.getScriptPath());
        assertEquals("", templateConfiguration.getScriptArgs());
        assertEquals("", templateConfiguration.getWorkingDirectoryPath());
        assertEquals(Collections.emptyMap(), templateConfiguration.getEnvs());
        assertEquals(true, templateConfiguration.isPassParentEnvs());
    }
}