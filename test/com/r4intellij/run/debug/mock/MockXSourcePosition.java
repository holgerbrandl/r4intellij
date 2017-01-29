package com.r4intellij.run.debug.mock;

import com.intellij.mock.MockVirtualFile;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.Navigatable;
import com.intellij.xdebugger.XSourcePosition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class MockXSourcePosition implements XSourcePosition {

    @Nullable
    private final String myFilename;

    private final int myLine;


    public MockXSourcePosition(@Nullable final String filename, final int line) {
        myFilename = filename;
        myLine = line;
    }


    @Override
    public int getLine() {
        return myLine;
    }


    @Override
    public int getOffset() {
        throw new IllegalStateException("GetOffset shouldn't be called");
    }


    @NotNull
    @Override
    public VirtualFile getFile() {
        if (myFilename == null) {
            throw new IllegalStateException("GetFile shouldn't be called");
        } else {
            return new MockVirtualFile(myFilename);
        }
    }


    @NotNull
    @Override
    public Navigatable createNavigatable(@NotNull final Project project) {
        throw new IllegalStateException("CreateNavigatable shouldn't be called");
    }
}
