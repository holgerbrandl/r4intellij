package com.r4intellij;

import com.intellij.lang.Language;

public class RLanguage extends Language {
    public static RLanguage getInstance() {
        return (RLanguage) RFileType.INSTANCE.getLanguage();
    }


    @Override
    public boolean isCaseSensitive() {
        return true; // http://jetbrains-feed.appspot.com/message/372001
    }


    protected RLanguage() {
        super("R");
    }
}
