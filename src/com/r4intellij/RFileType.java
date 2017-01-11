package com.r4intellij;

import com.intellij.lang.Language;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class RFileType extends LanguageFileType {

  public static final RFileType INSTANCE = new RFileType();

  private RFileType() {
    this(new RLanguage());
  }

  public RFileType(@NotNull final Language language) {
    super(language);
  }

  @Override
  @NotNull
  public String getName() {
    return "R";
  }

  @Override
  @NotNull
  public String getDescription() {
    return "R scripts";
  }

  @Override
  @NotNull
  public String getDefaultExtension() {
    return "R";
  }

  @Override
  //@NotNull
  public Icon getIcon() {
    return IconLoader.findIcon("/icons/r_logo_16.png");
  }
}
