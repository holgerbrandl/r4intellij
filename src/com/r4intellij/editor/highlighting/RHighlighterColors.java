package com.r4intellij.editor.highlighting;

import com.intellij.openapi.editor.colors.TextAttributesKey;

import static com.intellij.openapi.editor.DefaultLanguageHighlighterColors.*;
import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

/**
 * RHighterColors description
 *
 * Created on 7/23/14.
 * @author HongKee Moon
 */
public class RHighlighterColors {
	/** Default style for regular comment started with # */
	public static final TextAttributesKey COMMENT_ATTR_KEY = createTextAttributesKey("R.COMMENT", LINE_COMMENT);

	/** Default style for keyword */
	public static final TextAttributesKey KEYWORD_ATTR_KEY = createTextAttributesKey("R.KEYWORD", KEYWORD);

	/** Default style for parentheses */
	public static final TextAttributesKey PAREN_ATTR_KEY = createTextAttributesKey("R.PAREN", PARENTHESES);

	/** Default style for braces */
	public static final TextAttributesKey BRACES_ATTR_KEY = createTextAttributesKey("R.BRACES", BRACES);

	/** Default style for brackets */
	public static final TextAttributesKey BRACKETS_ATTR_KEY = createTextAttributesKey("R.BRACKETS", BRACKETS);

	/** Default style for number */
	public static final TextAttributesKey NUMBER_ATTR_KEY = createTextAttributesKey("R.NUMBER", NUMBER);

	/** Default style for string */
	public static final TextAttributesKey STRING_ATTR_KEY = createTextAttributesKey("R.STRING", STRING);

	/** Default style for variable */
	public static final TextAttributesKey VARIABLE_ATTR_KEY = createTextAttributesKey("R.VARIABLE", LOCAL_VARIABLE);

	/** Default style for funcall */
	public static final TextAttributesKey FUNCALL_ATTR_KEY = createTextAttributesKey("R.FUNCALL", FUNCTION_CALL);
}
