package com.r4intellij.debugger;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.mock.IllegalROutputReceiver;
import com.r4intellij.debugger.mock.MockROutputReceiver;
import org.junit.Test;

import java.util.Collections;

import static com.r4intellij.debugger.RDebuggerStringUtils.*;
import static org.junit.Assert.assertEquals;

public class RDebuggerStringUtilsTest {

    @Test
    public void emptyErrorAppending() {
        appendError(
                new RExecutionResult("", RExecutionResultType.EMPTY, TextRange.EMPTY_RANGE, ""),
                new IllegalROutputReceiver()
        );
    }


    @Test
    public void emptyResultAppending() {
        appendResult(
                new RExecutionResult("abc", RExecutionResultType.RESPONSE, TextRange.EMPTY_RANGE, ""),
                new IllegalROutputReceiver()
        );
    }


    @Test
    public void ordinaryErrorAppending() {
        final MockROutputReceiver receiver = new MockROutputReceiver();

        appendError(
                new RExecutionResult("", RExecutionResultType.EMPTY, TextRange.EMPTY_RANGE, "error"),
                receiver
        );

        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error"), receiver.getErrors());
    }


    @Test
    public void ordinaryResultAppending() {
        final MockROutputReceiver receiver = new MockROutputReceiver();

        appendResult(
                new RExecutionResult("output", RExecutionResultType.RESPONSE, new TextRange(0, 3), ""),
                receiver
        );

        assertEquals(Collections.singletonList("out"), receiver.getOutputs());
        assertEquals(Collections.emptyList(), receiver.getErrors());
    }


    @Test
    public void oneLineNextLineBegin() {
        final String line = "abc";

        assertEquals(line.length(), findNextLineBegin(line, 0));
    }


    @Test
    public void justLineBreaksNextLineBegin() {
        final String text = "\n\n\n\n\n";

        assertEquals(text.length(), findNextLineBegin(text, 0));
    }


    @Test
    public void lineBreakPointerNextLineBegin() {
        final String text = "abc\n\ndef";

        assertEquals(5, findNextLineBegin(text, 3));
    }


    @Test
    public void ordinaryNextLineBegin() {
        final String text = "abc\ndef";

        assertEquals(4, findNextLineBegin(text, 0));
    }


    @Test
    public void oneLineCurrentLineEnd() {
        final String line = "abc";

        assertEquals(line.length(), findCurrentLineEnd(line, 0));
    }


    @Test
    public void justLineBreaksCurrentLineEnd() {
        final String text = "\n\n\n\n";

        assertEquals(0, findCurrentLineEnd(text, 0));
    }


    @Test
    public void lineBreakPointerCurrentLineEnd() {
        final String text = "abc\n\ndef";

        assertEquals(3, findCurrentLineEnd(text, 3));
    }


    @Test
    public void ordinaryCurrentLineEnd() {
        final String text = "abc\ndef";

        assertEquals(3, findCurrentLineEnd(text, 0));
    }
}