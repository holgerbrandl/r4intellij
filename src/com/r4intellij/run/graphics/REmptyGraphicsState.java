package com.r4intellij.run.graphics;

import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

public class REmptyGraphicsState implements RGraphicsState {

    @NotNull
    private static final String NO_SNAPSHOTS = "No snapshots";

    @NotNull
    private static final String CURRENT_SNAPSHOT_WAS_NOT_SET = "Current snapshot wasn't set";

    @NotNull
    private final List<Listener> myListeners = new LinkedList<Listener>();


    @Override
    public boolean hasNext() {
        return false;
    }


    @Override
    public boolean hasPrevious() {
        return false;
    }


    @Override
    public boolean hasCurrent() {
        return false;
    }


    @Override
    public void next() {
        throw new NoSuchElementException(NO_SNAPSHOTS);
    }


    @Override
    public void previous() {
        throw new NoSuchElementException(NO_SNAPSHOTS);
    }


    @NotNull
    @Override
    public VirtualFile current() throws FileNotFoundException {
        throw new NoSuchElementException(CURRENT_SNAPSHOT_WAS_NOT_SET);
    }


    @Override
    public int size() {
        return 0;
    }


    @Override
    public void refresh(final boolean asynchronous) {
    }


    @Override
    public void reset() {
        for (final Listener listener : myListeners) {
            listener.onReset();
        }
    }


    @Override
    public void addListener(@NotNull final Listener listener) {
        myListeners.add(listener);
    }


    @Override
    public void removeListener(@NotNull final Listener listener) {
        myListeners.remove(listener);
    }
}
