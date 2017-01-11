package com.r4intellij;

import com.intellij.openapi.fileTypes.FileTypeConsumer;
import com.intellij.openapi.fileTypes.FileTypeFactory;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class RFileTypeFactory extends FileTypeFactory {
  @Override
  public void createFileTypes(@NonNls @NotNull final FileTypeConsumer consumer) {
    consumer.consume(RFileType.INSTANCE, "r");
  }
}