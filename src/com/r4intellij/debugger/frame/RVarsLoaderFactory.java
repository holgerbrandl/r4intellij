package com.r4intellij.debugger.frame;

import org.jetbrains.annotations.NotNull;

public interface RVarsLoaderFactory {

    @NotNull
    RVarsLoader getLoader(@NotNull final RValueModifier modifier, final int frameNumber);
}
