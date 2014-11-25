package org.eclipse.jdt.internal.formatter.redesign;

import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_LINE;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_BLOCK;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_JAVADOC;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameNotAToken;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameStringLiteral;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameWHITESPACE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.jdt.internal.compiler.parser.ScannerHelper;
import org.eclipse.jdt.internal.formatter.DefaultCodeFormatterOptions;
import org.eclipse.jdt.internal.formatter.redesign.Token.WrapPolicy;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;

public class TextEditsBuilder extends TokenTraverser {

	private static final char[] BREAK_CHARS = { '\r', '\n' };

	private final String source;
	private List<IRegion> regions;
	private TokenManager tm;
	private final DefaultCodeFormatterOptions options;
	private final StringBuilder buffer;

	private final List<Token> stringLiteralsInLine = new ArrayList<Token>();
	private final List<TextEdit> edits = new ArrayList<TextEdit>();

	private int currentRegion = 0;

	private TextEditsBuilder childBuilder;
	private final TextEditsBuilder parent;
	private int alignChar;
	private int sourceLimit;
	private int parentTokenIndex;

	public TextEditsBuilder(String source, IRegion[] regions, TokenManager tokenManager,
			DefaultCodeFormatterOptions options) {
		this.source = source;
		this.tm = tokenManager;
		this.options = options;
		this.regions = adaptRegions(regions);

		this.alignChar = this.options.tab_char;
		this.sourceLimit = source.length();
		this.parent = null;

		this.buffer = new StringBuilder();
	}

	private TextEditsBuilder(TextEditsBuilder parent) {
		this.buffer = parent.buffer;
		this.parent = parent;
		this.source = parent.source;
		this.options = parent.options;
		this.regions = parent.regions;
		this.alignChar = DefaultCodeFormatterOptions.SPACE;
	}

	private List<IRegion> adaptRegions(IRegion[] givenRegions) {
		if (givenRegions == null)
			return null;
		// make sure regions don't begin or end inside multiline comments
		ArrayList<IRegion> result = new ArrayList<IRegion>();
		IRegion previous = null;
		for (IRegion region : givenRegions) {
			int start = region.getOffset();
			int end = start + region.getLength() - 1;
			int sourceStart = this.tm.get(0).originalStart;

			if (start > sourceStart) {
				Token token = this.tm.get(this.tm.findIndex(start, -1, false));
				if ((token.tokenType == TokenNameCOMMENT_BLOCK || token.tokenType == TokenNameCOMMENT_JAVADOC)
						&& start <= token.originalEnd) {
					start = token.originalStart;
				}
			}

			if (end > start && end > sourceStart) {
				Token token = this.tm.get(this.tm.findIndex(end, -1, false));
				if ((token.tokenType == TokenNameCOMMENT_BLOCK || token.tokenType == TokenNameCOMMENT_JAVADOC)
						&& end < token.originalEnd) {
					end = token.originalEnd;
				}
			}

			if (previous != null && previous.getOffset() + previous.getLength() >= start) {
				result.remove(result.size() - 1);
				start = previous.getOffset();
			}
			if (end + 1 == this.source.length())
				end++;
			IRegion adapted = new Region(start, end - start + 1);
			result.add(adapted);
			previous = adapted;
		}
		return result;
	}

	@Override
	protected boolean token(Token token, int index) {

		bufferWhitespaceBefore(token, index);

		List<Token> structure = token.getInternalStructure();
		if (token.tokenType == TokenNameCOMMENT_LINE) {
			handleSingleLineComment(token, index);
		} else if (structure != null && !structure.isEmpty()) {
			handleMultiLineComment(token, index);
		} else {
			flushBuffer(token.originalStart);
			this.counter = token.originalEnd + 1;
		}

		if (token.tokenType == TokenNameStringLiteral)
			this.stringLiteralsInLine.add(token);

		if (getNext() == null) {
			for (int i = 0; i < token.getLineBreaksAfter(); i++)
				bufferLineSeparator(null, i + 1 == token.getLineBreaksAfter());
			char lastChar = this.source.charAt(this.sourceLimit - 1);
			if (token.getLineBreaksAfter() == 0 && (lastChar == '\r' || lastChar == '\n'))
				bufferLineSeparator(null, false);
			flushBuffer(this.sourceLimit);
		}
		return true;
	}

	private void bufferWhitespaceBefore(Token token, int index) {
		if (getLineBreaksBefore() > 0) {
			this.stringLiteralsInLine.clear();
			if (getLineBreaksBefore() > 1) {
				Token indentToken = null;
				if (this.options.indent_empty_lines && token.tokenType != TokenNameNotAToken)
					indentToken = token.getIndent() > getPrevious().getIndent() ? token : getPrevious();
				for (int i = 1; i < getLineBreaksBefore(); i++) {
					bufferLineSeparator(token, true);
					if (indentToken != null)
						bufferIndent(indentToken, index);
				}
			}
			bufferLineSeparator(token, false);
			bufferIndent(token, index);
		} else if (index == 0 && this.parent == null) {
			bufferIndent(token, index);
		}

		int align = token.getAlign();
		if (align > 0) {
			int positionInLine;
			if (getLineBreaksBefore() > 0) {
				positionInLine = this.tm.toIndent(token.getIndent(), token.getWrapPolicy() != null);
			} else {
				positionInLine = this.tm.getPositionInLine(index - 1);
				positionInLine += this.tm.getLength(this.tm.get(index - 1), positionInLine);
			}
			bufferAlign(positionInLine, align);
		} else if (isSpaceBefore() && getLineBreaksBefore() == 0 && index > 0) {
			this.buffer.append(' ');
		}

	}

	private void bufferLineSeparator(Token token, boolean emptyLine) {
		if (this.parent == null) {
			this.buffer.append(this.options.line_separator);
			return;
		}

		this.parent.counter = this.counter;
		this.parent.bufferLineSeparator(null, false);
		this.counter = this.parent.counter;

		bufferIndent(this.parent.tm.get(this.parentTokenIndex), -1);

		if (token != null && token.tokenType == TokenNameNotAToken)
			return; // this is an unformatted block comment, don't force asterisk

		if (getNext() == null && !emptyLine)
			return; // this is the last token of block comment, asterisk is included

		boolean asteriskFound = false;
		int searchLimit = token != null ? token.originalStart : this.sourceLimit;
		for (int i = this.counter; i < searchLimit; i++) {
			char c = this.source.charAt(i);
			if (c == '*') {
				this.buffer.append(' ');
				flushBuffer(i);
				while (i + 1 < this.sourceLimit && this.source.charAt(i + 1) == '*')
					i++;
				this.counter = i + 1;
				c = this.source.charAt(i + 1);
				if ((c != '\r' && c != '\n') || !emptyLine)
					this.buffer.append(' ');
				asteriskFound = true;
				break;
			}
			if (!ScannerHelper.isWhitespace(c))
				break;
		}
		if (!asteriskFound)
			this.buffer.append(" * "); //$NON-NLS-1$
	}

	private void bufferIndent(Token token, int index) {
		int indent = token.getIndent();
		int additionalSpaces = 0;
		if (this.options.use_tabs_only_for_leading_indentations) {
			// use indentation of wrap-line start token and add spaces to match current token
			WrapPolicy wrapPolicy = token.getWrapPolicy();
			int wrapRootIndent = indent;
			if (index == -1) { // this means we print a line separator in a multi-line comment 
				TokenManager tm2 = this.parent.tm;
				wrapRootIndent = tm2.get(tm2.findFirstTokenInLine(this.parentTokenIndex, true)).getIndent();
			} else if (wrapPolicy != null) {
				wrapRootIndent = this.tm.get(this.tm.findFirstTokenInLine(index, true)).getIndent();
			}
			additionalSpaces = indent - wrapRootIndent;
			indent = wrapRootIndent;

			if (wrapPolicy != null && wrapPolicy.isForced) {
				int extraIndent = wrapPolicy.extraIndent * this.options.indentation_size;
				additionalSpaces -= extraIndent;
				indent += extraIndent;
			}
		}
		appendIndentationString(this.buffer, this.options.tab_char, this.options.tab_size, indent,
				additionalSpaces);
	}

	public static void appendIndentationString(StringBuilder target, int tabChar, int tabSize, int indent,
			int additionalSpaces) {
		int spacesCount = additionalSpaces;
		int tabsCount = 0;
		switch (tabChar) {
			case DefaultCodeFormatterOptions.SPACE:
				spacesCount += indent;
				break;
			case DefaultCodeFormatterOptions.TAB:
				if (tabSize > 0) {
					tabsCount += indent / tabSize;
					if (indent % tabSize > 0)
						tabsCount++;
				}
				break;
			case DefaultCodeFormatterOptions.MIXED:
				if (tabSize > 0) {
					tabsCount += indent / tabSize;
					spacesCount += indent % tabSize;
				} else {
					spacesCount += indent;
				}
				break;
		}

		char[] indentChars = new char[tabsCount + spacesCount];
		Arrays.fill(indentChars, 0, tabsCount, '\t');
		Arrays.fill(indentChars, tabsCount, indentChars.length, ' ');
		target.append(indentChars);
	}

	private void bufferAlign(int currentPositionInLine, int align) {
		final int tabSize = this.options.tab_size;
		switch (this.alignChar) {
			case DefaultCodeFormatterOptions.SPACE:
				while (currentPositionInLine++ < align) {
					this.buffer.append(' ');
				}
				break;
			case DefaultCodeFormatterOptions.TAB:
				while (currentPositionInLine < align && tabSize > 0) {
					this.buffer.append('\t');
					currentPositionInLine += tabSize - currentPositionInLine % tabSize;
				}
				break;
			case DefaultCodeFormatterOptions.MIXED:
				while (tabSize > 0 && currentPositionInLine + tabSize - currentPositionInLine % tabSize <= align) {
					this.buffer.append('\t');
					currentPositionInLine += tabSize - currentPositionInLine % tabSize;
				}
				while (currentPositionInLine++ < align) {
					this.buffer.append(' ');
				}
		}
	}

	private void flushBuffer(int currentPosition) {
		String buffered = this.buffer.toString();
		boolean sourceMatch = this.source.startsWith(buffered, this.counter)
				&& this.counter + buffered.length() == currentPosition;
		if (!sourceMatch && checkRegions(this.counter, currentPosition)) {
			TextEdit edit = getReplaceEdit(this.counter, currentPosition, buffered);
			this.edits.add(edit);
		}
		this.buffer.setLength(0);
		this.counter = currentPosition;
	}

	private boolean checkRegions(int editStart, int editEnd) {
		while (true) {
			if (this.currentRegion >= this.regions.size())
				return false;
			IRegion region = this.regions.get(this.currentRegion);
			if (editEnd < region.getOffset())
				return false;
			if (editStart < region.getOffset() + region.getLength())
				return true;
			this.currentRegion++;
		}
	}

	private TextEdit getReplaceEdit(int editStart, int editEnd, String text) {
		IRegion region = this.regions.get(this.currentRegion);
		if (region.getOffset() > editStart) {
			int breaksInReplacement = this.tm.countLineBreaksBetween(text, 0, text.length());
			int breaksOutsideRegion = this.tm.countLineBreaksBetween(this.source, editStart, region.getOffset());
			int breaksToPreserve = breaksInReplacement - breaksOutsideRegion;
			text = adaptReplaceText(text, breaksToPreserve, false);
			if (breaksToPreserve <= 0) {
				int indentInSource = this.tm.findSourcePositionInLine(region.getOffset());
				int cutPosition = 0;
				int textPosition = 0;
				while (cutPosition < text.length() && textPosition < indentInSource) {
					textPosition += (text.charAt(cutPosition) == '\t') ? this.options.tab_size : 1;
					if (textPosition <= indentInSource)
						cutPosition++;
				}
				text = text.substring(cutPosition);
			}
			editStart = region.getOffset();
		}
		int regionEnd = region.getOffset() + region.getLength();
		if (regionEnd < editEnd) {
			int breaksInReplacement = this.tm.countLineBreaksBetween(text, 0, text.length());
			int breaksOutsideRegion = this.tm.countLineBreaksBetween(this.source, regionEnd, editEnd);
			int breaksToPreserve = breaksInReplacement - breaksOutsideRegion;
			text = adaptReplaceText(text, breaksToPreserve, true);
			editEnd = regionEnd;
		}
		return new ReplaceEdit(editStart, editEnd - editStart, text);
	}

	private String adaptReplaceText(String text, int breaksToPreserve, boolean preserveFrontLines) {
		int i = preserveFrontLines ? 0 : text.length() - 1;
		int direction = preserveFrontLines ? 1 : -1;
		int preservedBreaks = 0;
		theLoop:
		while (i >= 0 && i < text.length()) {
			for (int j = 0; j < 2; j++) {
				if (text.charAt(i) == BREAK_CHARS[j]) {
					if (preservedBreaks >= breaksToPreserve)
						break theLoop;
					preservedBreaks++;
					int i2 = i + direction;
					if (i2 >= 0 && i2 < text.length() && text.charAt(i2) == BREAK_CHARS[1 - j])
						i = i2;
				}
			}
			i += direction;
		}
		if (preserveFrontLines)
			return text.substring(0, i);
		return text.substring(i + 1, text.length());
	}

	private void handleSingleLineComment(Token lineComment, int index) {
		List<Token> structure = lineComment.getInternalStructure();
		if (structure == null) {
			flushBuffer(lineComment.originalStart);
			this.counter = lineComment.originalEnd + 1;
			return;
		}
		if (structure.get(0).tokenType == TokenNameWHITESPACE) {
			flushBuffer(structure.get(0).originalStart);
		} else {
			flushBuffer(lineComment.originalStart);
		}

		for (int i = 0; i < structure.size(); i++) {
			Token fragment = structure.get(i);

			if (fragment.getLineBreaksBefore() > 0) {
				bufferLineSeparator(fragment, false);
				bufferIndent(fragment, index);
			} else if (fragment.isSpaceBefore() && i > 0) {
				this.buffer.append(' ');
			}

			if (fragment.hasNLSTag()) {
				int tagNumber = this.stringLiteralsInLine.indexOf(fragment.getNLSTag());
				assert tagNumber >= 0;
				this.buffer.append("//$NON-NLS-").append(tagNumber + 1).append("$"); //$NON-NLS-1$ //$NON-NLS-2$
			}  else if (fragment.originalStart < this.counter) {
				// Comment line prefix may be a copy of earlier code
				this.buffer.append(this.tm.toString(fragment));
			} else {
				flushBuffer(fragment.originalStart);
				this.counter = fragment.originalEnd + 1;
			}
		}

		if (lineComment.originalEnd > lineComment.originalStart) // otherwise it's a forged comment
			flushBuffer(lineComment.originalEnd + 1);
	}

	private void handleMultiLineComment(Token comment, int index) {
		flushBuffer(comment.originalStart);
		if (this.childBuilder == null) {
			this.childBuilder = new TextEditsBuilder(this);
		}
		this.childBuilder.traverseInternalStructure(comment, index);
		this.edits.addAll(this.childBuilder.edits);
		this.childBuilder.edits.clear();
		this.counter = this.childBuilder.sourceLimit;
	}

	private void traverseInternalStructure(Token token, int index) {
		List<Token> structure = token.getInternalStructure();
		this.tm = new TokenManager(structure, this.parent.tm);
		this.counter = token.originalStart;
		this.sourceLimit = token.originalEnd + 1;

		this.parentTokenIndex = index;

		traverse(structure, 0);
	}

	public void processComment(Token commentToken) {
		assert commentToken.isComment();
		if (commentToken.tokenType == TokenNameCOMMENT_LINE) {
			handleSingleLineComment(commentToken, this.tm.indexOf(commentToken));
		} else {
			handleMultiLineComment(commentToken, this.tm.indexOf(commentToken));
		}
	}

	public List<TextEdit> getEdits() {
		return this.edits;
	}

	public void setAlignChar(int alignChar) {
		this.alignChar = alignChar;
	}
}