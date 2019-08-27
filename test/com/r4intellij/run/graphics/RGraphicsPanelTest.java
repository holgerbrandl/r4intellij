package com.r4intellij.run.graphics;

import com.intellij.mock.MockVirtualFile;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import static org.junit.Assert.fail;
import static org.mockito.Mockito.*;

public class RGraphicsPanelTest {

    @Test
    public void refreshNonexistent() throws FileNotFoundException {
        final RGraphicsState state = mock(RGraphicsState.class);

        when(state.current()).thenThrow(FileNotFoundException.class);

        final RGraphicsPanel panel = new RGraphicsPanel(state);

        try {
            panel.refresh();
        } catch (final AssertionError ignore) {
      /*
      state.current() generates FileNotFoundException,
      panel catches it and logs as error,
      but logger generates AssertionError with FileNotFoundException as a cause
      */
        }

        verify(state, times(1)).current();
        verifyNoMoreInteractions(state);
    }


    @Test
    public void refreshInvalid() throws FileNotFoundException {
        final RGraphicsState state = mock(RGraphicsState.class);

        when(state.current()).thenReturn(new TextVirtualFile("abc.txt", "text"));

        final RGraphicsPanel panel = new RGraphicsPanel(state);

        try {
            panel.refresh();

            fail();
        } catch (final IllegalStateException e) {
            verify(state, times(1)).current();
            verifyNoMoreInteractions(state);
        }
    }


    @Test
    public void reset() throws FileNotFoundException {
        final RGraphicsState state = mock(RGraphicsState.class);

        when(state.current()).thenThrow(FileNotFoundException.class);

        final RGraphicsPanel panel = new RGraphicsPanel(state);

        try {
            panel.refresh();
        } catch (final AssertionError ignore) {
      /*
      state.current() generates FileNotFoundException,
      panel catches it and logs as error,
      but logger generates AssertionError with FileNotFoundException as a cause
      */
        }

        verify(state, times(1)).current();

        panel.reset();

        verifyNoMoreInteractions(state);
    }


    private static class TextVirtualFile extends MockVirtualFile {

        public TextVirtualFile(@NotNull final String name, @NotNull final String text) {
            super(name, text);
        }


        @Override
        public InputStream getInputStream() {
            return new ByteArrayInputStream(contentsToByteArray());
        }
    }
}