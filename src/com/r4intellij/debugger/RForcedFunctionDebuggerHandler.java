package com.r4intellij.debugger;

import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.function.RFunctionDebugger;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import com.r4intellij.debugger.function.RFunctionDebuggerHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

class RForcedFunctionDebuggerHandler implements RFunctionDebuggerHandler {

    @NotNull
    private final List<RFunctionDebugger> myDebuggers;

    private int myDropFrames;

    @Nullable
    private String myResult;


    public RForcedFunctionDebuggerHandler(@NotNull final RExecutor executor,
                                          @NotNull final RFunctionDebuggerFactory factory,
                                          @NotNull final ROutputReceiver receiver) throws RDebuggerException {
        myDebuggers = new ArrayList<RFunctionDebugger>();
        myDropFrames = 1;

        appendDebugger(
                factory.getFunctionDebugger(
                        executor,
                        this,
                        receiver
                )
        );
    }


    public boolean advance() throws RDebuggerException {
        topDebugger().advance(); // Don't forget that advance could append new debugger

        while (!myDebuggers.isEmpty() && !topDebugger().hasNext()) {
            if (myDebuggers.size() == 1) {
                return false;
            }

            for (int i = 0; i < myDropFrames; i++) {
                popDebugger();
            }

            myDropFrames = 1;
        }

        return !myDebuggers.isEmpty();
    }


    @NotNull
    public String getResult() {
        if (myResult != null) {
            return myResult;
        } else {
            return topDebugger().getResult();
        }
    }


    @Override
    public void appendDebugger(@NotNull final RFunctionDebugger debugger) {
        myDebuggers.add(debugger);
    }


    @Override
    public void setReturnLineNumber(final int lineNumber) {
    }


    @Override
    public void setDropFrames(final int number) {
        myDropFrames = number;

        if (myDropFrames == myDebuggers.size()) {
            myResult = topDebugger().getResult();
        }
    }


    @NotNull
    private RFunctionDebugger topDebugger() {
        return myDebuggers.get(myDebuggers.size() - 1);
    }


    private void popDebugger() {
        myDebuggers.remove(myDebuggers.size() - 1);
    }
}
