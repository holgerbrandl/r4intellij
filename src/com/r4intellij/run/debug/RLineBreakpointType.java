package com.r4intellij.run.debug;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.xdebugger.breakpoints.XLineBreakpointTypeBase;
import org.jetbrains.annotations.NotNull;

public class RLineBreakpointType extends XLineBreakpointTypeBase {

    @NotNull
    private static final String ID = "the-r-line";

    @NotNull
    private static final String TITLE = "R Breakpoints";


    public RLineBreakpointType() {
        super(ID, TITLE, new REditorsProvider());
    }


    @Override
    public boolean canPutAt(@NotNull final VirtualFile file, final int line, @NotNull final Project project) {
        return RLineBreakpointUtils.canPutAt(project, file, line);
    }
}
