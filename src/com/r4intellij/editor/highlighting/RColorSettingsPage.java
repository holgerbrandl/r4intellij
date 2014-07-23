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
	/** The path to the sample .R file */
	@NonNls
	protected static final String SAMPLE_R_PATH = "/sample.R";

	/**
	 * The sample .R document shown in the colors settings dialog
	 *
	 * @see #loadSampleR()
	 */
	protected static final String SAMPLE_R = loadSampleR();

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
		return SAMPLE_R;
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

	/**
	 * Loads sample .R file
	 *
	 * @return the text loaded from {@link #SAMPLE_R_PATH}
	 * @see #getDemoText()
	 * @see #SAMPLE_R_PATH
	 * @see #SAMPLE_R
	 */
	protected static String loadSampleR() {
		return Resources.getResourceContent(SAMPLE_R_PATH);
	}
}
