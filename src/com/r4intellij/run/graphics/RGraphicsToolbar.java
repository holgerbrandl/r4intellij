package com.r4intellij.run.graphics;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.*;
import com.intellij.util.ui.JBUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

class RGraphicsToolbar {

    @NotNull
    private static final String TOOLBAR_PLACE = "Graphics";

    @NotNull
    private static final String PREVIOUS_GRAPHICS_ACTION_TEXT = "Previous graphics";

    @NotNull
    private static final String NEXT_GRAPHICS_ACTION_TEXT = "Next graphics";

    @NotNull
    private final JPanel myToolbar;


    public RGraphicsToolbar(@NotNull final RGraphicsState state, @NotNull final Listener listener) {
        myToolbar = createToolbar(state, listener);
    }


    @NotNull
    public JPanel getToolbar() {
        return myToolbar;
    }


    @NotNull
    private static JPanel createToolbar(@NotNull final RGraphicsState state, @NotNull final Listener listener) {
        final DefaultActionGroup actionGroup = new DefaultActionGroup();

        actionGroup.add(new PreviousGraphicsAction(state, listener));
        actionGroup.add(new NextGraphicsAction(state, listener));

        final ActionToolbar actionToolbar = ActionManager.getInstance().createActionToolbar(TOOLBAR_PLACE, actionGroup, true);

        return JBUI.Panels.simplePanel(actionToolbar.getComponent());
    }


    interface Listener {

        void next();


        void previous();
    }


    private static class PreviousGraphicsAction extends AnAction {

        @NotNull
        private final RGraphicsState myState;

        @NotNull
        private final Listener myListener;


        public PreviousGraphicsAction(@NotNull final RGraphicsState state, @NotNull final Listener listener) {
            super(PREVIOUS_GRAPHICS_ACTION_TEXT, PREVIOUS_GRAPHICS_ACTION_TEXT, AllIcons.Actions.Back);

            myState = state;
            myListener = listener;
        }


        @Override
        public void actionPerformed(final AnActionEvent e) {
            myListener.previous();
        }


        @Override
        public void update(final AnActionEvent e) {
            final boolean hasPrevious = myState.hasPrevious();

            if (e.getPresentation().isEnabled() ^ hasPrevious) {
                e.getPresentation().setEnabled(hasPrevious);
            }
        }
    }


    private static class NextGraphicsAction extends AnAction {

        @NotNull
        private final RGraphicsState myState;

        @NotNull
        private final Listener myListener;


        public NextGraphicsAction(@NotNull final RGraphicsState state, @NotNull final Listener listener) {
            super(NEXT_GRAPHICS_ACTION_TEXT, NEXT_GRAPHICS_ACTION_TEXT, AllIcons.Actions.Forward);

            myState = state;
            myListener = listener;
        }


        @Override
        public void actionPerformed(final AnActionEvent e) {
            myListener.next();
        }


        @Override
        public void update(final AnActionEvent e) {
            final boolean hasNext = myState.hasNext();

            if (e.getPresentation().isEnabled() ^ hasNext) {
                e.getPresentation().setEnabled(hasNext);
            }
        }
    }
}
