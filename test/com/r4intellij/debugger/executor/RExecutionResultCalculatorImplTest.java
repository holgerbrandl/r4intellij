package com.r4intellij.debugger.executor;

import com.r4intellij.debugger.data.RCommands;
import com.r4intellij.debugger.data.RLanguageConstants;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import static com.r4intellij.debugger.data.RCommands.EXECUTE_AND_STEP_COMMAND;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_ENTER_FUNCTION_SUFFIX;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_FUNCTION_PREFIX;
import static com.r4intellij.debugger.data.RResponseConstants.*;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static org.junit.Assert.*;

public class RExecutionResultCalculatorImplTest {

    @NotNull
    private static final String JETBRAINS_THER_X_ENTER = SERVICE_FUNCTION_PREFIX + "x" + SERVICE_ENTER_FUNCTION_SUFFIX;

    @NotNull
    private static final RExecutionResultCalculatorImpl CALCULATOR = new RExecutionResultCalculatorImpl();


    @Test
    public void completePlus() {
        assertTrue(CALCULATOR.isComplete("x <- function() {\n" + PLUS_AND_SPACE));
    }


    @Test
    public void completeBrowser() {
        assertTrue(CALCULATOR.isComplete("ls()\n[1] \"x\"\n" + BROWSE_PREFIX + "1" + BROWSE_SUFFIX));
    }


    @Test
    public void completeIncomplete() {
        assertFalse(CALCULATOR.isComplete("ls()\n[1] \"x\"\n" + BROWSE_PREFIX));
    }


    @Test
    public void calculatePlus() {
        check(
                "x <- function() {",
                "",
                PLUS_AND_SPACE,
                PLUS,
                ""
        );
    }


    @Test
    public void calculateJustBrowse() {
        check(
                RCommands.debugCommand("x"),
                "",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                EMPTY,
                ""
        );
    }


    @Test(expected = IllegalArgumentException.class)
    public void calculateIncomplete() {
        check(
                "ls()",
                "[1] \"x\"",
                BROWSE_PREFIX,
                RESPONSE,
                "[1] \"x\""
        );
    }


    @Test
    public void calculateDebuggingIn() {
        check(
                "x()",
                DEBUGGING_IN_PREFIX + "x()\n" +
                        DEBUG_AT_PREFIX + "{\n" +
                        "    .doTrace(" + JETBRAINS_THER_X_ENTER + "(), \"on entry\")\n" +
                        "    {\n" +
                        "        print(\"x\")\n" +
                        "    }\n" +
                        "}",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                DEBUGGING_IN,
                ""
        );
    }


    @Test
    public void calculateDebugAt() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                DEBUG_AT_LINE_PREFIX + "1: x <- c(1)",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                DEBUG_AT,
                ""
        );
    }


    @Test
    public void calculateUnbraceDebugAt() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                DEBUG_AT_PREFIX + "x <- c(1)",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                DEBUG_AT,
                ""
        );
    }


    @Test
    public void calculateUnbraceDebugAtWithOutput() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                "[1] 1 2 3\n" +
                        DEBUG_AT_PREFIX + "x <- c(1)",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                DEBUG_AT,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateDebugAtWithOutput() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                "[1] 1 2 3\n" +
                        DEBUG_AT_LINE_PREFIX + "1: x <- c(1)",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                DEBUG_AT,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateDebugAtFunction() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                DEBUG_AT_LINE_PREFIX + "2: x <- function() {\n" +
                        "print(\"x\")\n" +
                        "}",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                DEBUG_AT,
                ""
        );
    }


    @Test
    public void calculateStartTraceBraceTopLevel() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                TRACING_PREFIX + "x() on entry \n" +
                        "[1] \"x\"\n" +
                        DEBUG_AT_PREFIX + "{\n" +
                        "    print(\"x\")\n" +
                        "}",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                START_TRACE_BRACE,
                ""
        );
    }


    @Test
    public void calculateStartTraceBraceInside() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                TRACING_PREFIX + "f() on entry \n" +
                        "[1] \"f\"\n" +
                        DEBUG_AT_PREFIX + "for (i in 1:2) {\n" +
                        "    print(i)\n" +
                        "}",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                START_TRACE_BRACE,
                ""
        );
    }


    @Test
    public void calculateStartTraceUnbrace() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                TRACING_PREFIX + "x() on entry \n" +
                        "[1] \"x\"\n" +
                        DEBUG_AT_PREFIX + "print(\"x\")",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                START_TRACE_UNBRACE,
                ""
        );
    }


    @Test
    public void calculateContinueTrace() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[1L]], ...)\n" +
                        DEBUGGING_IN_PREFIX + "FUN(c(-1, 0, 1)[[2L]], ...)\n" +
                        DEBUG_AT_PREFIX + "{\n" +
                        "    .doTrace(" + JETBRAINS_THER_X_ENTER + "(), \"on entry\")\n" +
                        "    {\n" +
                        "        print(\"x\")\n" +
                        "    }\n" +
                        "}",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                CONTINUE_TRACE,
                ""
        );
    }


    @Test
    public void calculateContinueTraceWithOutputBefore() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[1L]], ...)\n" +
                        DEBUGGING_IN_PREFIX + "FUN(c(-1, 0, 1)[[2L]], ...)\n" +
                        DEBUG_AT_PREFIX + "{\n" +
                        "    .doTrace(" + JETBRAINS_THER_X_ENTER + "(), \"on entry\")\n" +
                        "    {\n" +
                        "        print(\"x\")\n" +
                        "    }\n" +
                        "}",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                CONTINUE_TRACE,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateContinueTraceWithOutputAfter() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[1L]], ...)\n" +
                        "[1] 1 2 3\n" +
                        DEBUGGING_IN_PREFIX + "FUN(c(-1, 0, 1)[[2L]], ...)\n" +
                        DEBUG_AT_PREFIX + "{\n" +
                        "    .doTrace(" + JETBRAINS_THER_X_ENTER + "(), \"on entry\")\n" +
                        "    {\n" +
                        "        print(\"x\")\n" +
                        "    }\n" +
                        "}",
                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                CONTINUE_TRACE,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateExitingFrom() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.EXITING_FROM,
                ""
        );
    }


    @Test
    public void calculateExitingFromWithOutputBefore() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateExitingFromWithOutputAfter() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        "[1] 1 2 3",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateExitingFromWithOutputBeforeAndDebugAt() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        DEBUG_AT_LINE_PREFIX + "1: x <- c(1)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateExitingFromWithOutputAfterAndDebugAt() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        "[1] 1 2 3\n" +
                        DEBUG_AT_LINE_PREFIX + "1: x <- c(1)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateRecursiveExitingFrom() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RECURSIVE_EXITING_FROM,
                ""
        );
    }


    @Test
    public void calculateRecursiveExitingFromWithOutputBefore() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RECURSIVE_EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateRecursiveExitingFromWithOutputInside() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "foo()\n" +
                        "[1] 2 3 4 5 6\n" +
                        EXITING_FROM_PREFIX + "bar()",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RECURSIVE_EXITING_FROM,
                "[1] 2 3 4 5 6"
        );
    }


    @Test
    public void calculateRecursiveExitingFromWithOutputAfter() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        "[1] 1 2 3",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RECURSIVE_EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateRecursiveExitingFromWithOutputBeforeAndDebugAt() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        DEBUG_AT_LINE_PREFIX + "1: x <- c(1)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RECURSIVE_EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateRecursiveExitingFromWithOutputInsideAndDebugAt() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "foo()\n" +
                        "[1] 2 3 4 5 6\n" +
                        EXITING_FROM_PREFIX + "bar()\n" +
                        DEBUG_AT_LINE_PREFIX + "1: x <- c(1)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RECURSIVE_EXITING_FROM,
                "[1] 2 3 4 5 6"
        );
    }


    @Test
    public void calculateRecursiveExitingFromWithOutputAfterAndDebugAt() {
        check(
                EXECUTE_AND_STEP_COMMAND,
                EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
                        "[1] 1 2 3\n" +
                        DEBUG_AT_LINE_PREFIX + "1: x <- c(1)",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RECURSIVE_EXITING_FROM,
                "[1] 1 2 3"
        );
    }


    @Test
    public void calculateOutputAndBrowse() {
        check(
                "ls()",
                "[1] \"x\"",
                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RESPONSE,
                "[1] \"x\""
        );
    }


    private void check(@NotNull final String command,
                       @NotNull final String expectedOutput,
                       @NotNull final String tail,
                       @NotNull final RExecutionResultType expectedType,
                       @NotNull final String expectedResult) {
        final RExecutionResult result = CALCULATOR.calculate(
                calculateFinalCommand(command, expectedOutput, tail),
                ""
        );

        assertEquals(expectedOutput, result.getOutput());
        assertEquals(expectedType, result.getType());
        assertEquals(expectedResult, result.getResultRange().substring(result.getOutput()));
        assertEquals("", result.getError());
    }


    private String calculateFinalCommand(@NotNull final String command,
                                         @NotNull final String expectedOutput,
                                         @NotNull final String tail) {
        final StringBuilder sb = new StringBuilder();

        sb.append(command);
        sb.append(RLanguageConstants.LINE_SEPARATOR);

        if (!expectedOutput.isEmpty()) {
            sb.append(expectedOutput);
            sb.append(RLanguageConstants.LINE_SEPARATOR);
        }

        sb.append(tail);

        return sb.toString();
    }
}
