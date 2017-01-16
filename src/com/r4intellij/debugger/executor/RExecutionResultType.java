package com.r4intellij.debugger.executor;

public enum RExecutionResultType {
    PLUS,
    EMPTY,
    DEBUGGING_IN,
    DEBUG_AT,
    START_TRACE_BRACE,
    START_TRACE_UNBRACE,
    CONTINUE_TRACE,
    EXITING_FROM,
    RECURSIVE_EXITING_FROM,
    RESPONSE
}
