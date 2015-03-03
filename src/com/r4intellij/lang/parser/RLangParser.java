package com.r4intellij.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

/**
 * Created by moon on 7/25/14.
 */
public class RLangParser implements PsiParser {
	/**
	 * Parse the contents of the specified PSI builder and returns an AST tree with the
	 * specified type of root element.
	 *
	 * @param root    the type of the root element in the AST tree.
	 * @param builder the builder which is used to retrieve the original file tokens and build the AST tree.
	 * @return the root of the resulting AST tree.
	 */
	@NotNull
	public ASTNode parse(IElementType root, PsiBuilder builder) {

		PsiBuilder.Marker rootMarker = builder.mark();

		// TODO Actual parsing not implemented

		while (!builder.eof()) {
			builder.advanceLexer();
		}

		rootMarker.done(root);

		return builder.getTreeBuilt();
	}
}
