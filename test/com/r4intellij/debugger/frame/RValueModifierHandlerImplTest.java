package com.r4intellij.debugger.frame;

import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class RValueModifierHandlerImplTest {

    @Test
    public void ordinary() {
        final RValueModifierHandlerImpl handler = new RValueModifierHandlerImpl();

        handler.setLastFrameNumber(2);

        assertTrue(handler.isModificationAvailable(2));
        assertFalse(handler.isModificationAvailable(1));
    }
}