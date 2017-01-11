package com.r4intellij.run.configuration;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.testFramework.PlatformTestCase;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.XMLOutputter;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.StringReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;

public class RRunConfigurationTest extends PlatformTestCase {

  @NotNull
  private static final String SCRIPT_PATH = "s_path";

  @NotNull
  private static final String SCRIPT_ARGS = "s_args";

  @NotNull
  private static final String WORKING_DIRECTORY_PATH = "w_path";

  @NotNull
  private static final Map<String, String> ENVS = calculateEnvs();

  private static final boolean PASS_PARENT_ENVS = true;

  @NotNull
  private static final String ELEMENT_NAME = "CONFIGURATION";

  @NotNull
  private static final String SCRIPT_PATH_OPTION = "SCRIPT_PATH";

  @NotNull
  private static final String SCRIPT_ARGS_OPTION = "SCRIPT_ARGS";

  @NotNull
  private static final String WORKING_DIRECTORY_PATH_OPTION = "WORKING_DIRECTORY_PATH";

  @NotNull
  private static final String PASS_PARENT_ENVS_OPTION = "PASS_PARENT_ENVS";

  private static final String ENVS_ELEMENT = "<envs><env name=\"k1\" value=\"v\" /></envs>";

  @NotNull
  private static final ConfigurationFactory CONFIGURATION_FACTORY = mock(ConfigurationFactory.class);

  public void testEnvsImmutability() {
    final HashMap<String, String> envsCopy = new HashMap<String, String>(ENVS);
    final RRunConfiguration runConfiguration = new RRunConfiguration(getProject(), CONFIGURATION_FACTORY);

    runConfiguration.setEnvs(envsCopy);
    assertEquals(envsCopy, runConfiguration.getEnvs());

    envsCopy.put("k2", "v");
    assertEquals(ENVS, runConfiguration.getEnvs());
  }

  public void testReadExternal() throws JDOMException, IOException, InvalidDataException {
    final RRunConfiguration runConfiguration = new RRunConfiguration(getProject(), CONFIGURATION_FACTORY);

    final Element element = new SAXBuilder().build(
      new StringReader(
        "<" + ELEMENT_NAME + ">" +
        "<option name=\"" + SCRIPT_PATH_OPTION + "\" value=\"" + SCRIPT_PATH + "\" />" +
        "<option name=\"" + SCRIPT_ARGS_OPTION + "\" value=\"" + SCRIPT_ARGS + "\" />" +
        "<option name=\"" + WORKING_DIRECTORY_PATH_OPTION + "\" value=\"" + WORKING_DIRECTORY_PATH + "\" />" +
        "<option name=\"" + PASS_PARENT_ENVS_OPTION + "\" value=\"" + PASS_PARENT_ENVS + "\" />" +
        ENVS_ELEMENT +
        "</" + ELEMENT_NAME + ">"
      )
    ).getRootElement();

    runConfiguration.readExternal(element);

    assertEquals(SCRIPT_PATH, runConfiguration.getScriptPath());
    assertEquals(SCRIPT_ARGS, runConfiguration.getScriptArgs());
    assertEquals(WORKING_DIRECTORY_PATH, runConfiguration.getWorkingDirectoryPath());
    assertEquals(ENVS, runConfiguration.getEnvs());
    assertEquals(PASS_PARENT_ENVS, runConfiguration.isPassParentEnvs());
  }

  public void testWriteExternal() throws WriteExternalException {
    final RRunConfiguration runConfiguration = new RRunConfiguration(getProject(), CONFIGURATION_FACTORY);
    runConfiguration.setScriptPath(SCRIPT_PATH);
    runConfiguration.setScriptArgs(SCRIPT_ARGS);
    runConfiguration.setWorkingDirectoryPath(WORKING_DIRECTORY_PATH);
    runConfiguration.setEnvs(ENVS);
    runConfiguration.setPassParentEnvs(PASS_PARENT_ENVS);

    final Element element = new Element(ELEMENT_NAME);

    runConfiguration.writeExternal(element);

    assertEquals(
      "<" + ELEMENT_NAME + ">" +
      "<option name=\"" + SCRIPT_PATH_OPTION + "\" value=\"" + SCRIPT_PATH + "\" />" +
      "<option name=\"" + SCRIPT_ARGS_OPTION + "\" value=\"" + SCRIPT_ARGS + "\" />" +
      "<option name=\"" + WORKING_DIRECTORY_PATH_OPTION + "\" value=\"" + WORKING_DIRECTORY_PATH + "\" />" +
      "<option name=\"" + PASS_PARENT_ENVS_OPTION + "\" value=\"" + PASS_PARENT_ENVS + "\" />" +
      ENVS_ELEMENT +
      "</" + ELEMENT_NAME + ">",
      new XMLOutputter().outputString(element)
    );
  }

  public void testCopyParams() {
    final RRunConfiguration runConfiguration1 = new RRunConfiguration(getProject(), CONFIGURATION_FACTORY);
    runConfiguration1.setScriptPath(SCRIPT_PATH);
    runConfiguration1.setScriptArgs(SCRIPT_ARGS);
    runConfiguration1.setWorkingDirectoryPath(WORKING_DIRECTORY_PATH);
    runConfiguration1.setEnvs(ENVS);
    runConfiguration1.setPassParentEnvs(PASS_PARENT_ENVS);

    final RRunConfiguration runConfiguration2 = new RRunConfiguration(getProject(), CONFIGURATION_FACTORY);

    RRunConfiguration.copyParams(runConfiguration1, runConfiguration2);

    assertEquals(SCRIPT_PATH, runConfiguration2.getScriptPath());
    assertEquals(SCRIPT_ARGS, runConfiguration2.getScriptArgs());
    assertEquals(WORKING_DIRECTORY_PATH, runConfiguration2.getWorkingDirectoryPath());
    assertEquals(ENVS, runConfiguration2.getEnvs());
    assertEquals(true, runConfiguration2.isPassParentEnvs());
  }

  @NotNull
  private static Map<String, String> calculateEnvs() {
    final Map<String, String> envs = new HashMap<String, String>();
    envs.put("k1", "v");

    return Collections.unmodifiableMap(envs);
  }
}