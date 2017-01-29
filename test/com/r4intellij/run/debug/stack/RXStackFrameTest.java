package com.r4intellij.run.debug.stack;

import com.intellij.icons.AllIcons;
import com.intellij.ui.ColoredTextContainer;
import com.intellij.ui.SimpleTextAttributes;
import com.intellij.xdebugger.frame.XValueChildrenList;
import com.r4intellij.debugger.data.RFunctionConstants;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.frame.RStackFrame;
import com.r4intellij.debugger.frame.RVar;
import com.r4intellij.debugger.frame.RVarsLoader;
import com.r4intellij.debugger.mock.IllegalRDebuggerEvaluator;
import com.r4intellij.debugger.mock.IllegalRValueModifier;
import com.r4intellij.debugger.mock.IllegalRVarsLoader;
import com.r4intellij.run.debug.mock.ExecutorServices;
import com.r4intellij.run.debug.mock.IllegalXCompositeNode;
import com.r4intellij.run.debug.mock.MockXSourcePosition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Test;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

public class RXStackFrameTest {

    @Test
    public void ordinaryComputing() {
        final OrdinaryRVarsLoader loader = new OrdinaryRVarsLoader();
        final OrdinaryXCompositeNode node = new OrdinaryXCompositeNode();

        final RXStackFrame frame = new RXStackFrame(
                new RStackFrame(
                        new RLocation("abc", 2),
                        loader,
                        new IllegalRDebuggerEvaluator()
                ),
                null,
                ExecutorServices.SINGLE_EXECUTOR
        );

        frame.computeChildren(node);

        //noinspection StatementWithEmptyBody
        while (loader.myCounter == 0 || node.myCounter == 0) {
        }

        assertEquals(1, loader.myCounter);
        assertEquals(1, node.myCounter);
    }


    @Test
    public void errorComputing() {
        final ErrorRVarsLoader loader = new ErrorRVarsLoader();
        final ErrorXCompositeNode node = new ErrorXCompositeNode();

        final RXStackFrame frame = new RXStackFrame(
                new RStackFrame(
                        new RLocation("def", 4),
                        loader,
                        new IllegalRDebuggerEvaluator()
                ),
                null,
                ExecutorServices.SINGLE_EXECUTOR
        );

        frame.computeChildren(node);

        //noinspection StatementWithEmptyBody
        while (loader.myCounter == 0 || node.myCounter == 0) {
        }

        assertEquals(1, loader.myCounter);
        assertEquals(1, node.myCounter);
    }


    @Test
    public void nullPresentation() {
        final RXStackFrame frame = new RXStackFrame(
                new RStackFrame(
                        new RLocation("abc", 2),
                        new IllegalRVarsLoader(),
                        new IllegalRDebuggerEvaluator()
                ),
                null,
                ExecutorServices.ILLEGAL_EXECUTOR
        );

        final MockColoredTextContainer container = new MockColoredTextContainer();

        frame.customizePresentation(container);

        assertEquals(Collections.singletonList("<invalid frame>"), container.myFragments);
        assertEquals(Collections.singletonList(SimpleTextAttributes.ERROR_ATTRIBUTES), container.myAttributes);
        assertEquals(null, container.myIcon);
    }


    @Test
    public void mainPresentation() {
        final RXStackFrame frame = new RXStackFrame(
                new RStackFrame(
                        new RLocation(RFunctionConstants.MAIN_FUNCTION_NAME, 2),
                        new IllegalRVarsLoader(),
                        new IllegalRDebuggerEvaluator()
                ),
                new MockXSourcePosition("script.r", 2),
                ExecutorServices.ILLEGAL_EXECUTOR
        );

        final MockColoredTextContainer container = new MockColoredTextContainer();

        frame.customizePresentation(container);

        assertEquals(Arrays.asList("script.r", ":3"), container.myFragments);
        assertEquals(Arrays.asList(SimpleTextAttributes.REGULAR_ATTRIBUTES, SimpleTextAttributes.REGULAR_ATTRIBUTES), container.myAttributes);
        assertEquals(AllIcons.Debugger.StackFrame, container.myIcon);
    }


    @Test
    public void ordinaryPresentation() {
        final RXStackFrame frame = new RXStackFrame(
                new RStackFrame(
                        new RLocation("abc", 2),
                        new IllegalRVarsLoader(),
                        new IllegalRDebuggerEvaluator()
                ),
                new MockXSourcePosition("script.r", 9),
                ExecutorServices.ILLEGAL_EXECUTOR
        );

        final MockColoredTextContainer container = new MockColoredTextContainer();

        frame.customizePresentation(container);

        assertEquals(Collections.singletonList("abc, script.r:10"), container.myFragments);
        assertEquals(Collections.singletonList(SimpleTextAttributes.REGULAR_ATTRIBUTES), container.myAttributes);
        assertEquals(AllIcons.Debugger.StackFrame, container.myIcon);
    }


    private static class OrdinaryRVarsLoader implements RVarsLoader {

        private int myCounter = 0;


        @NotNull
        @Override
        public List<RVar> load() throws RDebuggerException {
            myCounter++;

            return Arrays.asList(
                    new RVar("n1", "t1", "v1", new MockRValueModifier(true)),
                    new RVar("n2", "t2", "v2", new MockRValueModifier(false))
            );
        }
    }


    private static class OrdinaryXCompositeNode extends IllegalXCompositeNode {

        private int myCounter = 0;


        @Override
        public void addChildren(@NotNull final XValueChildrenList children, final boolean last) {
            myCounter++;

            assertEquals(2, children.size());
            assertTrue(last);

            assertEquals("n1", children.getName(0));
            assertEquals("n2", children.getName(1));

            assertNotNull(children.getValue(0).getModifier());
            assertNull(children.getValue(1).getModifier());
        }
    }


    private static class ErrorRVarsLoader implements RVarsLoader {

        private int myCounter = 0;


        @NotNull
        @Override
        public List<RVar> load() throws RDebuggerException {
            myCounter++;

            throw new RDebuggerException("");
        }
    }


    private static class ErrorXCompositeNode extends IllegalXCompositeNode {

        private int myCounter = 0;


        @Override
        public void setErrorMessage(@NotNull final String errorMessage) {
            myCounter++;
        }
    }


    private static class MockColoredTextContainer implements ColoredTextContainer {

        @NotNull
        private final List<String> myFragments = new ArrayList<String>();

        @NotNull
        private final List<SimpleTextAttributes> myAttributes = new ArrayList<SimpleTextAttributes>();

        @Nullable
        private Icon myIcon = null;


        @Override
        public void append(@NotNull final String fragment, @NotNull final SimpleTextAttributes attributes) {
            myFragments.add(fragment);
            myAttributes.add(attributes);
        }


        @Override
        public void append(@NotNull final String fragment, @NotNull final SimpleTextAttributes attributes, final Object tag) {
            throw new IllegalStateException("Append shouldn't be called");
        }


        @Override
        public void setIcon(@Nullable final Icon icon) {
            myIcon = icon;
        }


        @Override
        public void setToolTipText(@Nullable final String text) {
            throw new IllegalStateException("SetToolTipText shouldn't be called");
        }
    }


    private static class MockRValueModifier extends IllegalRValueModifier {

        private final boolean myIsEnabled;


        public MockRValueModifier(final boolean isEnabled) {
            myIsEnabled = isEnabled;
        }


        @Override
        public boolean isEnabled() {
            return myIsEnabled;
        }
    }
}