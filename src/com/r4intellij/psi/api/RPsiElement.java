package com.r4intellij.psi.api;

import com.intellij.psi.NavigatablePsiElement;

public interface RPsiElement extends NavigatablePsiElement {
    /**
     * An empty array to return cheaply without allocating it anew.
     */
    RPsiElement[] EMPTY_ARRAY = new RPsiElement[0];
}
