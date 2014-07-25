package com.r4intellij.lang.lexer;

import com.intellij.lexer.LayeredLexer;

import static com.r4intellij.psi.RTypes.*;

/**
 * Created by moon on 7/25/14.
 */
public class RHighlightingLexer extends LayeredLexer {
	public RHighlightingLexer()
	{
		super(new RLexer());
//		registerSelfStoppingLayer(new StringLiteralLexer('\"', JavaTokenType.STRING_LITERAL),
//				new IElementType[]{JavaTokenType.STRING_LITERAL}, IElementType.EMPTY_ARRAY);
//
//		registerSelfStoppingLayer(new StringLiteralLexer('\'', JavaTokenType.STRING_LITERAL),
//				new IElementType[]{JavaTokenType.CHARACTER_LITERAL}, IElementType.EMPTY_ARRAY);
	}
}
