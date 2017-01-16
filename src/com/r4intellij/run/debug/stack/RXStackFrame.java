package com.r4intellij.run.debug.stack;

import com.intellij.icons.AllIcons;
import com.intellij.ui.ColoredTextContainer;
import com.intellij.ui.SimpleTextAttributes;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.intellij.xdebugger.frame.*;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.frame.RStackFrame;
import com.r4intellij.debugger.frame.RVar;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.concurrent.ExecutorService;

import static com.r4intellij.debugger.data.RFunctionConstants.MAIN_FUNCTION_NAME;

class RXStackFrame extends XStackFrame {

    @NotNull
    private final RStackFrame myFrame;

    @Nullable
    private final XSourcePosition myPosition;

    @NotNull
    private final ExecutorService myExecutor;

    @Nullable
    private RXDebuggerEvaluator myEvaluator;


    public RXStackFrame(@NotNull final RStackFrame frame,
                        @Nullable final XSourcePosition position,
                        @NotNull final ExecutorService executor) {
        myPosition = position;
        myFrame = frame;
        myExecutor = executor;
        myEvaluator = null;
    }


    @Nullable
    @Override
    public XSourcePosition getSourcePosition() {
        return myPosition;
    }


    @NotNull
    @Override
    public XDebuggerEvaluator getEvaluator() {
        if (myEvaluator == null) {
            myEvaluator = new RXDebuggerEvaluator(myFrame.getEvaluator(), myExecutor);
        }

        return myEvaluator;
    }


    @Override
    public void computeChildren(@NotNull final XCompositeNode node) {
        myExecutor.execute(
                new Runnable() {
                    @Override
                    public void run() {
                        try {
                            node.addChildren(
                                    transform(
                                            myFrame.getLoader().load()
                                    ),
                                    true
                            );
                        } catch (final RDebuggerException e) {
                            node.setErrorMessage(e.getMessage());
                        }
                    }
                }
        );
    }


    @Override
    public void customizePresentation(@NotNull final ColoredTextContainer component) {
        if (myPosition == null || myFrame.getLocation().getFunctionName().equals(MAIN_FUNCTION_NAME)) {
            super.customizePresentation(component);
        } else {
            component.append(getPresentationText(), SimpleTextAttributes.REGULAR_ATTRIBUTES);
            component.setIcon(AllIcons.Debugger.StackFrame);
        }
    }


    @NotNull
    private XValueChildrenList transform(@NotNull final List<RVar> vars) {
        final XValueChildrenList result = new XValueChildrenList();

        for (final RVar var : vars) {
            result.add(new RXVar(var, myExecutor));
        }

        return result;
    }


    @NotNull
    private String getPresentationText() {
        assert myPosition != null; // see method usages

        return myFrame.getLocation().getFunctionName() + ", " + myPosition.getFile().getName() + ":" + (myPosition.getLine() + 1);
    }


    private static class RXVar extends XNamedValue {

        @NotNull
        private final RVar myVar;

        @NotNull
        private final ExecutorService myExecutor;


        public RXVar(@NotNull final RVar var, @NotNull final ExecutorService executor) {
            super(var.getName());

            myVar = var;
            myExecutor = executor;
        }


        @Override
        public void computePresentation(@NotNull final XValueNode node, @NotNull final XValuePlace place) {
            RXPresentationUtils.computePresentation(myVar, node);
        }


        @Nullable
        @Override
        public XValueModifier getModifier() {
            if (myVar.getModifier().isEnabled()) {
                return new RXValueModifier(myVar.getModifier(), myVar.getName(), myExecutor);
            } else {
                return null;
            }
        }
    }
}
