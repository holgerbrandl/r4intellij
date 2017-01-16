package com.r4intellij.typing.types;

/**
 * Actually this class implements bottom type from type theory
 */
public class RErrorType extends RType {
    private final String myErrorMessage;


    public RErrorType(String errorMessage) {
        myErrorMessage = errorMessage;
    }


    @Override
    public String getCanonicalName() {
        return "error: " + myErrorMessage;
    }


    public String getErrorMessage() {
        return myErrorMessage;
    }
}
