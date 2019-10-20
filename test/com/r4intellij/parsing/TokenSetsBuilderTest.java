package com.r4intellij.parsing;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.r4intellij.psi.RElementType;
import kotlin.test.AssertionsKt;
import org.junit.Test;

import java.util.Arrays;

public class TokenSetsBuilderTest {

    private static final IElementType T1 = new RElementType("1");
    private static final IElementType T2 = new RElementType("2");
    private static final IElementType T3 = new RElementType("3");
    private static final IElementType T4 = new RElementType("4");

    @Test
    public void setIncluding() {
        TokenSetsBuilder b = new TokenSetsBuilder();
        int s0 = b.set();
        int s1 = b.setIncluding(s0);
        int s2 = b.set();
        int s3 = b.setIncluding(s1, s2);

        b.classify(T1, s0);
        b.classify(T2, s1);
        b.classify(T3, s2);
        b.classify(T4, s3);

        TokenSet tokens1 = b.get(s0);
        TokenSet tokens2 = b.get(s1);
        TokenSet tokens3 = b.get(s2);
        TokenSet tokens4 = b.get(s3);

        assertTokens(tokens1, T1);
        assertTokens(tokens2, T1, T2);
        assertTokens(tokens3, T3);
        assertTokens(tokens4, T1, T2, T3, T4);
    }

    private static void assertTokens(TokenSet set, IElementType... expected) {
        AssertionsKt.assertEquals(Arrays.asList(expected), Arrays.asList(set.getTypes()), null);
    }
}
