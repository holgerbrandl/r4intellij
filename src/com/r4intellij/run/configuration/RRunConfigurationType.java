package com.r4intellij.run.configuration;

import com.intellij.execution.configurations.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;

public class RRunConfigurationType extends ConfigurationTypeBase {

  public RRunConfigurationType() {
    super(
      "RRunConfigurationType",
      "R",
      "R run configuration",
      IconLoader.getIcon("/icons/r_logo_16.png")
    );

    addFactory(new RConfigurationFactory(this));
  }

  @NotNull
  public static RRunConfigurationType getInstance() {
    return ConfigurationTypeUtil.findConfigurationType(RRunConfigurationType.class);
  }

  @NotNull
  public ConfigurationFactory getMainFactory() {
    return getConfigurationFactories()[0];
  }

  private static class RConfigurationFactory extends ConfigurationFactory {

    public RConfigurationFactory(@NotNull final ConfigurationType configurationType) {
      super(configurationType);
    }

    @NotNull
    @Override
    public RunConfiguration createTemplateConfiguration(@NotNull final Project project) {
      return new RRunConfiguration(project, this);
    }
  }
}
