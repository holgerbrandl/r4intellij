package com.r4intellij.run.debug.stack;

import com.r4intellij.debugger.frame.RVar;
import com.r4intellij.debugger.mock.IllegalRValueModifier;
import com.r4intellij.run.debug.mock.MockXValueNode;
import com.r4intellij.run.debug.mock.MockXVarNode;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class RXPresentationUtilsTest {

    @Test
    public void oneLineVar() {
        final MockXVarNode node = new MockXVarNode("tp", "vl", null);

        RXPresentationUtils.computePresentation(
                new RVar("nm", "tp", "vl", new IllegalRValueModifier()),
                node
        );

        assertEquals(1, node.getPres());
        assertEquals(0, node.getEval());
    }


    @Test
    public void multilineVar() {
        final MockXVarNode node = new MockXVarNode("tp", "m l t", " m  l   t    \nvl");

        RXPresentationUtils.computePresentation(
                new RVar("nm", "tp", " m  l   t    \nvl", new IllegalRValueModifier()),
                node
        );

        assertEquals(1, node.getPres());
        assertEquals(1, node.getEval());
    }


    @Test
    public void oneLineValue() {
        final MockXValueNode node = new MockXValueNode("vl", null);

        RXPresentationUtils.computePresentation(
                "vl",
                node
        );

        assertEquals(1, node.getPres());
        assertEquals(0, node.getEval());
    }


    @Test
    public void multilineValue() {
        final MockXValueNode node = new MockXValueNode("m l t", " m  l   t    \nvl");

        RXPresentationUtils.computePresentation(
                " m  l   t    \nvl",
                node
        );

        assertEquals(1, node.getPres());
        assertEquals(1, node.getEval());
    }
}