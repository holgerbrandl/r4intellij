package com.r4intellij.parsing;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;

import java.util.*;

public final class TokenSetsBuilder {

    private final Map<IElementType, BitSet> map = new LinkedHashMap<>();
    private final List<BitSet> alsoIn = new ArrayList<>();
    private int n;


    public int set() {
        return setIncluding();
    }

    public int setIncluding(int... includedSets) {
        for (int i = 0; i < n; i++) {
            for (int zw : includedSets) {
                if (alsoIn.get(i).get(zw))
                    alsoIn.get(i).set(n);
            }
        }
        alsoIn.add(bitset(n));
        return n++;
    }

    public void classify(IElementType token, int... sets) {
        if (map.containsKey(token))
            throw new IllegalArgumentException("token is already registered");
        BitSet bits = bitset();
        for (int set : sets)
            bits.or(alsoIn.get(set));
        map.put(token, bits);
    }

    public TokenSet get(int setId) {
        List<IElementType> tokens = new ArrayList<>();
        map.forEach((key, value) -> {
            if (value.get(setId)) tokens.add(key);
        });
        return TokenSet.create(tokens.toArray(new IElementType[0]));
    }

    private static BitSet bitset(int... bits) {
        BitSet bitSet = new BitSet();
        for (int set : bits) bitSet.set(set);
        return bitSet;
    }
}
