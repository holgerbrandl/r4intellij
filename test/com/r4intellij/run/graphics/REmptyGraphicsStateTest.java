package com.r4intellij.run.graphics;

import org.junit.Test;
import org.mockito.Mockito;

import java.io.FileNotFoundException;
import java.util.NoSuchElementException;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class REmptyGraphicsStateTest {

    @Test(expected = NoSuchElementException.class)
    public void empty() throws FileNotFoundException {
        final REmptyGraphicsState state = new REmptyGraphicsState();

        assertFalse(state.hasNext());
        assertFalse(state.hasPrevious());
        assertFalse(state.hasCurrent());
        assertEquals(0, state.size());

        state.current();
    }


    @Test
    public void movement() {
        final REmptyGraphicsState state = new REmptyGraphicsState();

        try {
            state.next();

            fail("State successfully moved forward");
        } catch (final NoSuchElementException ignore) {
        }

        try {
            state.previous();

            fail("State successfully moved backward");
        } catch (final NoSuchElementException ignore) {
        }
    }


    @Test
    public void reset() {
        final REmptyGraphicsState state = new REmptyGraphicsState();
        final RGraphicsState.Listener listener = Mockito.mock(RGraphicsState.Listener.class);

        state.addListener(listener);
        verifyZeroInteractions(listener);

        state.reset();

        verify(listener, times(1)).onReset();
        verifyNoMoreInteractions(listener);
    }


    @Test
    public void listener() {
        final REmptyGraphicsState state = new REmptyGraphicsState();
        final RGraphicsState.Listener listener = Mockito.mock(RGraphicsState.Listener.class);

        state.addListener(listener);
        state.removeListener(listener);

        state.reset();

        verifyZeroInteractions(listener);
    }
}