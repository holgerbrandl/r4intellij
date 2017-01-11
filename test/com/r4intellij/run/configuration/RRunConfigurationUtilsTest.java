package com.r4intellij.run.configuration;

import com.intellij.openapi.options.ConfigurationException;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.*;

public class RRunConfigurationUtilsTest {

  @Test(expected = ConfigurationException.class)
  public void checkConfigurationWithoutScriptPath() throws ConfigurationException {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("");

    RRunConfigurationUtils.checkConfiguration(runConfiguration);
  }

  @Test(expected = ConfigurationException.class)
  public void checkConfigurationWithoutWorkingDirectoryPath() throws ConfigurationException {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getWorkingDirectoryPath()).thenReturn("");

    RRunConfigurationUtils.checkConfiguration(runConfiguration);
  }

  @Test
  public void checkConfiguration() throws ConfigurationException {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("s_p");
    when(runConfiguration.getWorkingDirectoryPath()).thenReturn("w_d_p");

    RRunConfigurationUtils.checkConfiguration(runConfiguration);

    verify(runConfiguration, times(1)).getScriptPath();
    verify(runConfiguration, times(1)).getWorkingDirectoryPath();
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void suggestedNameForUnknownScriptPath() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("");

    assertNull(RRunConfigurationUtils.suggestedName(runConfiguration));

    verify(runConfiguration, times(1)).getScriptPath();
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void suggestedNameForNotRScript() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("script.s");

    assertEquals("script.s", RRunConfigurationUtils.suggestedName(runConfiguration));

    verify(runConfiguration, times(1)).getScriptPath();
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void suggestedNameForRScript() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("script.r");

    assertEquals("script", RRunConfigurationUtils.suggestedName(runConfiguration));

    verify(runConfiguration, times(1)).getScriptPath();
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void suggestedWorkingDirectoryPathForUnknownScriptPath() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("");

    assertNull(RRunConfigurationUtils.suggestedWorkingDirectoryPath(runConfiguration));

    verify(runConfiguration, times(1)).getScriptPath();
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void suggestedWorkingDirectoryPath() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("home/script.r");

    assertEquals("home", RRunConfigurationUtils.suggestedWorkingDirectoryPath(runConfiguration));

    verify(runConfiguration, times(1)).getScriptPath();
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void setSuggestedWorkingDirectoryPathWhenNotSpecified() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("home/script.r");
    when(runConfiguration.getWorkingDirectoryPath()).thenReturn("");

    RRunConfigurationUtils.setSuggestedWorkingDirectoryPathIfNotSpecified(runConfiguration);

    verify(runConfiguration, times(1)).getScriptPath();
    verify(runConfiguration, times(1)).getWorkingDirectoryPath();
    verify(runConfiguration, times(1)).setWorkingDirectoryPath("home");
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void setSuggestedWorkingDirectoryPathWhenSpecified() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getWorkingDirectoryPath()).thenReturn("home");

    RRunConfigurationUtils.setSuggestedWorkingDirectoryPathIfNotSpecified(runConfiguration);

    verify(runConfiguration, times(1)).getWorkingDirectoryPath();
    verifyNoMoreInteractions(runConfiguration);
  }

  @Test
  public void setSuggestedWorkingDirectoryPathWhenCouldNotBeSuggested() {
    final RRunConfiguration runConfiguration = mock(RRunConfiguration.class);

    when(runConfiguration.getScriptPath()).thenReturn("");
    when(runConfiguration.getWorkingDirectoryPath()).thenReturn("");

    RRunConfigurationUtils.setSuggestedWorkingDirectoryPathIfNotSpecified(runConfiguration);

    verify(runConfiguration, times(1)).getScriptPath();
    verify(runConfiguration, times(1)).getWorkingDirectoryPath();
    verifyNoMoreInteractions(runConfiguration);
  }
}