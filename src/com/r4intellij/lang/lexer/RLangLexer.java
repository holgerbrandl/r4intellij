/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang.lexer;

import java.io.Reader;

import com.intellij.lexer.LexerBase;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.text.CharArrayUtil;
import gnu.trove.THashSet;

import java.io.IOException;
import java.util.Set;

import com.r4intellij.psi.RTypes;


public class RLangLexer extends LexerBase {

	private _RLexer myFlexLexer;
	private final HashTable myTable;
	private static final HashTable ourTableWithoutAssert = new HashTable();

	private CharSequence myBuffer;
	private char[] myBufferArray;
	private int myBufferIndex;
	private int myBufferEndOffset;

	private IElementType myTokenType;
	//Positioned after the last symbol of the current token
	private int myTokenEndOffset;

    public RLangLexer()
	{
		myFlexLexer = new _RLexer((Reader) null);
		myTable = getTable();
	}

	private static HashTable getTable() {
		return ourTableWithoutAssert;
	}

	public static boolean isKeyword(String id) {
		return getTable().contains(id);
	}

	private static final class HashTable {
		private static final int NUM_ENTRIES = 999;
		private static final Logger LOG = Logger.getInstance("com.r4intellij.lang.lexer.RLangLexer");

		private final char[][] myTable = new char[NUM_ENTRIES][];
		private final IElementType[] myKeywords = new IElementType[NUM_ENTRIES];
		private final Set<String> myKeywordsInSet = new THashSet<String>();

		private void add(String s, IElementType tokenType) {
			char[] chars = s.toCharArray();
			int hashCode = chars[0] * 2;
			for (int j = 1; j < chars.length; j++) {
				hashCode += chars[j];
			}
			int modHashCode = hashCode % NUM_ENTRIES;
			LOG.assertTrue(myTable[modHashCode] == null);

			myTable[modHashCode] = chars;
			myKeywords[modHashCode] = tokenType;
			myKeywordsInSet.add(s);
		}

		public boolean contains(String s) {
			return myKeywordsInSet.contains(s);
		}

		private boolean contains(int hashCode, final char[] bufferArray, final CharSequence buffer, int offset) {
			int modHashCode = hashCode % NUM_ENTRIES;
			final char[] kwd = myTable[modHashCode];
			if (kwd == null) return false;

			if (bufferArray != null) {
				for (int j = 0; j < kwd.length; j++) {
					if (bufferArray[j + offset] != kwd[j]) return false;
				}
			} else {
				for (int j = 0; j < kwd.length; j++) {
					if (buffer.charAt(j + offset) != kwd[j]) return false;
				}
			}
			return true;
		}

		private IElementType getTokenType(int hashCode) {
			return myKeywords[hashCode % NUM_ENTRIES];
		}

		@SuppressWarnings({"HardCodedStringLiteral"})
		private HashTable() {
			add("function", RTypes.R_FUNCTION);
			add("for", RTypes.R_FOR);
			add("while", RTypes.R_WHILE);
			add("if", RTypes.R_IF);
			add("else", RTypes.R_ELSE);
			add("break", RTypes.R_BREAK);
			add("next", RTypes.R_NEXT);
			add("repeat", RTypes.R_REPEAT);
			add("in", RTypes.R_IN);
			add("NULL", RTypes.R_NULL_CONST);
			add("...", RTypes.R_SYMBOL_FORMALS);
		}
	}
	@Override
	public final void start(CharSequence buffer, int startOffset, int endOffset, int initialState) {
		myBuffer = buffer;
		myBufferArray = CharArrayUtil.fromSequenceWithoutCopying(buffer);
		myBufferIndex = startOffset;
		myBufferEndOffset = endOffset;
		myTokenType = null;
		myTokenEndOffset = startOffset;
		myFlexLexer.reset(myBuffer, startOffset, endOffset, 0);
	}

	@Override
	public int getState() {
		return 0;
	}

	@Override
	public final IElementType getTokenType() {
		locateToken();

		return myTokenType;
	}

	@Override
	public final int getTokenStart() {
		return myBufferIndex;
	}

	@Override
	public final int getTokenEnd() {
		locateToken();
		return myTokenEndOffset;
	}


	@Override
	public final void advance() {
		locateToken();
		myTokenType = null;
	}

	protected final void locateToken() {
		if (myTokenType != null) return;
		_locateToken();
	}

	private void _locateToken() {
		if (myTokenEndOffset == myBufferEndOffset) {
			myTokenType = null;
			myBufferIndex = myBufferEndOffset;
			return;
		}

		myBufferIndex = myTokenEndOffset;

		final char c = myBufferArray != null ? myBufferArray[myBufferIndex]:myBuffer.charAt(myBufferIndex);

//		final char nextChar = myBufferArray != null ? myBufferArray[myBufferIndex + 1]:myBuffer.charAt(myBufferIndex + 1);
//		if(nextChar == '(')
//		{
//			myTokenType = RTypes.R_FUNCALL;
//			myTokenEndOffset = myBufferIndex + 1;
//			myBufferIndex = getFunctionName(myBufferIndex);
//			if (myTokenEndOffset > myBufferEndOffset) {
//				myTokenEndOffset = myBufferEndOffset;
//			}
//			return;
//		}

		switch (c) {
			default:
				flexLocateToken();
				break;

//			case '(':
//				myTokenType = RTypes.R_FUNCALL;
//				myTokenEndOffset = myBufferEndOffset;
//				myBufferIndex = getFunctionName(myBufferIndex);
//				break;

//			case '/':
//				if (myBufferIndex + 1 >= myBufferEndOffset) {
//					myTokenType = JavaTokenType.DIV;
//					myTokenEndOffset = myBufferEndOffset;
//				}
//				else {
//					final char nextChar = myBufferArray != null ? myBufferArray[myBufferIndex + 1]:myBuffer.charAt(myBufferIndex + 1);
//
//					if (nextChar == '/') {
//						myTokenType = JavaTokenType.END_OF_LINE_COMMENT;
//						myTokenEndOffset = getLineTerminator(myBufferIndex + 2);
//					}
//					else if (nextChar == '*') {
//						if (myBufferIndex + 2 >= myBufferEndOffset ||
//								(myBufferArray != null ? myBufferArray[myBufferIndex + 2]:myBuffer.charAt(myBufferIndex + 2)) != '*' ||
//								(myBufferIndex + 3 < myBufferEndOffset &&
//										(myBufferArray != null ? myBufferArray[myBufferIndex + 3]:myBuffer.charAt(myBufferIndex + 3)) == '/')) {
//							myTokenType = JavaTokenType.C_STYLE_COMMENT;
//							myTokenEndOffset = getClosingComment(myBufferIndex + 2);
//						}
//						else {
//							myTokenType = JavaDocElementType.DOC_COMMENT;
//							myTokenEndOffset = getClosingComment(myBufferIndex + 3);
//						}
//					}
//					else if (c > 127 && Character.isJavaIdentifierStart(c)) {
//						myTokenEndOffset = getIdentifier(myBufferIndex + 1);
//					}
//					else {
//						flexLocateToken();
//					}
//				}
//				break;
//
//			case '"':
//			case '\'':
//				myTokenType = c == '"' ? JavaTokenType.STRING_LITERAL : JavaTokenType.CHARACTER_LITERAL;
//				myTokenEndOffset = getClosingParenthesis(myBufferIndex + 1, c);
		}

		if (myTokenEndOffset > myBufferEndOffset) {
			myTokenEndOffset = myBufferEndOffset;
		}
	}

	private int getWhitespaces(int pos) {
		if (pos >= myBufferEndOffset) return myBufferEndOffset;
		final CharSequence lBuffer = myBuffer;
		final char[] lBufferArray = myBufferArray;

		char c = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);

		while (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f') {
			pos++;
			if (pos == myBufferEndOffset) return pos;
			c = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);
		}

		return pos;
	}

	private void flexLocateToken() {
		try {
			myFlexLexer.goTo(myBufferIndex);
			myTokenType = myFlexLexer.advance();
			myTokenEndOffset = myFlexLexer.getTokenEnd();

			if(myTokenType == RTypes.R_SYMBOL)
			{
				int function = getFunction(myTokenEndOffset);
				if(function < myBufferEndOffset)
				{
					final char nextChar = myBufferArray != null ? myBufferArray[function] : myBuffer.charAt(function);
					if (nextChar == '(')
						myTokenType = RTypes.R_FUNCALL;
					else
						myTokenType = RTypes.R_VARIABLE;
				}
			}
		}
		catch (IOException e) {
			// Can't be
		}
	}


	private int getFunction(int offset)
	{
		int pos = offset;
		final int lBufferEnd = myBufferEndOffset;
		if (pos >= lBufferEnd) return lBufferEnd;

		final CharSequence lBuffer = myBuffer;
		final char[] lBufferArray = myBufferArray;
		char cur = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);

		while (cur == '\n' || cur == '\r' || cur == ' ' || cur == '\t') {
			pos++;
			if (pos >= lBufferEnd) return lBufferEnd;
			cur = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);
		}

		return pos;
	}

	private int getClosingParenthesis(int offset, char c) {
		int pos = offset;
		final int lBufferEnd = myBufferEndOffset;
		if (pos >= lBufferEnd) return lBufferEnd;

		final CharSequence lBuffer = myBuffer;
		final char[] lBufferArray = myBufferArray;
		char cur = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);

		while (true) {
			while (cur != c && cur != '\n' && cur != '\r' && cur != '\\') {
				pos++;
				if (pos >= lBufferEnd) return lBufferEnd;
				cur = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);
			}

			if (cur == '\\') {
				pos++;
				if (pos >= lBufferEnd) return lBufferEnd;
				cur = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);
				if (cur == '\n' || cur == '\r') continue;
				pos++;
				if (pos >= lBufferEnd) return lBufferEnd;
				cur = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);
			}
			else if (cur == c) {
				break;
			}
			else {
				pos--;
				break;
			}
		}

		return pos + 1;
	}

	private int getClosingComment(int offset) {
		int pos = offset;

		final int lBufferEnd = myBufferEndOffset;
		final CharSequence lBuffer = myBuffer;
		final char[] lBufferArray = myBufferArray;

		while (pos < lBufferEnd - 1) {
			final char c = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);

			if (c == '*' && (lBufferArray != null ? lBufferArray[pos + 1]:lBuffer.charAt(pos + 1)) == '/') {
				break;
			}
			pos++;
		}

		return pos + 2;
	}

	private int getLineTerminator(int offset) {
		int pos = offset;
		final int lBufferEnd = myBufferEndOffset;
		final CharSequence lBuffer = myBuffer;
		final char[] lBufferArray = myBufferArray;

		while (pos < lBufferEnd) {
			final char c = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);
			if (c == '\r' || c == '\n') break;
			pos++;
		}

		return pos;
	}

	private int getIdentifier(int offset) {
		final CharSequence lBuffer = myBuffer;
		final char[] lBufferArray = myBufferArray;

		int hashCode = (lBufferArray != null ? lBufferArray[offset - 1]:lBuffer.charAt(offset - 1)) * 2;
		final int lBufferEnd = myBufferEndOffset;

		int pos = offset;
		if (pos < lBufferEnd) {
			char c = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);

			while (c >= 'a' && c <= 'z' ||
					c >= 'A' && c <= 'Z' ||
					c >= '0' && c <= '9' ||
					c == '.' ||	c == '_') {
				pos++;
				hashCode += c;

				if (pos == lBufferEnd) break;
				c = lBufferArray != null ? lBufferArray[pos]:lBuffer.charAt(pos);
			}
		}

		if (myTable.contains(hashCode, lBufferArray, lBuffer, offset - 1)) {
			myTokenType = myTable.getTokenType(hashCode);
		}
		else {
			myTokenType = RTypes.R_VARIABLE;
		}

		return pos;
	}

	@Override
	public CharSequence getBufferSequence() {
		return myBuffer;
	}

	@Override
	public final int getBufferEnd() {
		return myBufferEndOffset;
	}

  /*
  public static void main(String[] args) throws IOException {
    File root = new File(args[0]);

    Stats stats = new Stats();
    walk(root, stats);

    System.out.println("Scanned " + stats.files + " files, total of " + stats.lines + " lines in " + (stats.time / 1000000) + " ms.");
    System.out.println("Size:" + stats.bytes);

  }

  private static void lex(File root, Stats stats) throws IOException {
    stats.files++;
    BufferedReader reader = new BufferedReader(new FileReader(root));
    String s;
    StringBuilder buf = new StringBuilder();
    while ((s = reader.readLine()) != null) {
      stats.lines++;
      buf.append(s).append("\n");
    }

    stats.bytes += buf.length();

    long start = System.nanoTime();
    lexText(buf);
    stats.time += System.nanoTime() - start;
  }

  private static void lexText(StringBuilder buf) {
    JavaLexer lexer = new JavaLexer(LanguageLevel.JDK_1_5);
    lexer.start(buf);
    while (lexer.getTokenType() != null) {
      lexer.advance();
    }
  }

  private static class Stats {
    public int files;
    public int lines;
    public long time;
    public long bytes;
  }

  private static void walk(File root, Stats stats) throws IOException {
    if (root.isDirectory()) {
      System.out.println("Lexing in " + root.getPath());
      for (File file : root.listFiles()) {
        walk(file, stats);
      }
    }
    else {
      if (root.getName().endsWith(".java")) {
        lex(root, stats);
      }
    }
  }
  */

}
