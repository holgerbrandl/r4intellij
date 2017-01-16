package com.r4intellij.run.graphics;

import com.intellij.openapi.ui.SimpleToolWindowPanel;
import org.jetbrains.annotations.NotNull;

class RGraphicsToolWindow extends SimpleToolWindowPanel implements RGraphicsState.Listener {

    @NotNull
    private final RGraphicsState myState;

    @NotNull
    private final RGraphicsPanel myPanel;


    public RGraphicsToolWindow(@NotNull final RGraphicsState state) {
        super(true, true);

        myState = state;

        if (!myState.hasCurrent()) {
            while (myState.hasNext()) {
                myState.next();
            }
        }

        myPanel = new RGraphicsPanel(myState);

        if (myState.hasCurrent()) {
            myPanel.refresh();
        }

        setToolbar(new RGraphicsToolbar(myState, new ToolbarListener()).getToolbar());
        setContent(myPanel.getPanel());

        myState.addListener(this);
    }


    @Override
    public void onAdd() {
        if (myState.hasNext()) {
            myState.next();
        }
    }


    @Override
    public void onCurrentChange() {
        myPanel.refresh();
    }


    @Override
    public void onReset() {
        myPanel.reset();
    }


    private class ToolbarListener implements RGraphicsToolbar.Listener {

        @Override
        public void next() {
            myState.next();
        }


        @Override
        public void previous() {
            myState.previous();
        }
    }
}
