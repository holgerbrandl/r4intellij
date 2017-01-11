package com.r4intellij.run.debug.mock;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.junit.Assert.assertEquals;

public class MockXValueTextRenderer implements XValuePresentation.XValueTextRenderer {

  @NotNull
  private final String myExpected;

  private int myCounter = 0;

  public MockXValueTextRenderer(@NotNull final String expected) {
    myExpected = expected;
  }

  @Override
  public void renderValue(@NotNull final String value) {
    myCounter++;

    assertEquals(myExpected, value);
  }

  @Override
  public void renderStringValue(@NotNull final String value) {
    throw new IllegalStateException("RenderStringValue shouldn't be called");
  }

  @Override
  public void renderNumericValue(@NotNull final String value) {
    throw new IllegalStateException("RenderNumericValue shouldn't be called");
  }

  @Override
  public void renderKeywordValue(@NotNull final String value) {
    throw new IllegalStateException("RenderKeywordValue shouldn't be called");
  }

  @Override
  public void renderValue(@NotNull final String value, @NotNull final TextAttributesKey key) {
    throw new IllegalStateException("RenderValue shouldn't be called");
  }

  @Override
  public void renderStringValue(@NotNull final String value,
                                @Nullable final String additionalSpecialCharsToHighlight,
                                final int maxLength) {
    throw new IllegalStateException("RenderStringValue shouldn't be called");
  }

  @Override
  public void renderComment(@NotNull final String comment) {
    throw new IllegalStateException("RenderComment shouldn't be called");
  }

  @Override
  public void renderSpecialSymbol(@NotNull final String symbol) {
    throw new IllegalStateException("RenderSpecialSymbol shouldn't be called");
  }

  @Override
  public void renderError(@NotNull final String error) {
    throw new IllegalStateException("RenderError shouldn't be called");
  }

  public int getCounter() {
    return myCounter;
  }
}
