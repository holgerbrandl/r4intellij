package com.r4intellij.editor.highlighting;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import com.intellij.openapi.util.IconLoader;
import com.r4intellij.lang.RLanguage;
import com.r4intellij.lang.RBundle;
import com.r4intellij.misc.Resources;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

/**
 * RColorSettingsPage implementation
 * Created on 7/23/14.
 * @author HongKee Moon
 */
public class RColorSettingsPage implements ColorSettingsPage {

	private static final AttributesDescriptor[] DESCRIPTORS = new AttributesDescriptor[]{
			new AttributesDescriptor(RBundle.message("highlighter.comment"), RHighlighterColors.COMMENT_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.keyword"), RHighlighterColors.KEYWORD_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.paren"), RHighlighterColors.PAREN_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.braces"), RHighlighterColors.BRACES_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.brackets"), RHighlighterColors.BRACKETS_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.number"), RHighlighterColors.NUMBER_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.string"), RHighlighterColors.STRING_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.variable"), RHighlighterColors.VARIABLE_ATTR_KEY),
			new AttributesDescriptor(RBundle.message("highlighter.funcall"), RHighlighterColors.FUNCALL_ATTR_KEY),
	};

	@Nullable
	@Override
	public Icon getIcon() {
		return IconLoader.findIcon("/icons/r16_icon.png");
	}

	@NotNull
	@Override
	public SyntaxHighlighter getHighlighter() {
		return new RSyntaxHighlighter(null, null);
	}

	@NotNull
	@Override
	public String getDemoText() {
		//return SAMPLE_R;
		return "\nfor (i in names(list)) {\n" +
				"    if(true)\n" +
				"    {\n" +
				"        #line comment\n" +
				"        names[,i] = 0\n" +
				"        names[,'added'] = \"string\"\n" +
				"    }\n" +
				"\n" +
				"    a = 1\n" +
				"    head(names)\n" +
				"}\n" +
				"head(a)";
	}

	@Nullable
	@Override
	public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
		return null;
	}

	@NotNull
	@Override
	public AttributesDescriptor[] getAttributeDescriptors() {
		return DESCRIPTORS;
	}

	@NotNull
	@Override
	public ColorDescriptor[] getColorDescriptors() {
		return ColorDescriptor.EMPTY_ARRAY;
	}

	@NotNull
	@Override
	public String getDisplayName() {
		return RLanguage.NAME;
	}
}
