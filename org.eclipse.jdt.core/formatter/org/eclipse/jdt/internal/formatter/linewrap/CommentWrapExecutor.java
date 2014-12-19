package org.eclipse.jdt.internal.formatter.linewrap;

import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_JAVADOC;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_LINE;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameNotAToken;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameWHITESPACE;
import static org.eclipse.jdt.internal.formatter.redesign.CommentsPreparator.COMMENT_LINE_SEPARATOR_LENGTH;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jdt.internal.formatter.DefaultCodeFormatterOptions;
import org.eclipse.jdt.internal.formatter.redesign.Token;
import org.eclipse.jdt.internal.formatter.redesign.Token.WrapPolicy;
import org.eclipse.jdt.internal.formatter.redesign.TokenManager;
import org.eclipse.jdt.internal.formatter.redesign.TokenTraverser;

public class CommentWrapExecutor extends TokenTraverser {

	private final TokenManager tm;
	private final DefaultCodeFormatterOptions options;

	private final ArrayList<Token> nlsTags = new ArrayList<Token>();

	private int lineStartPosition;
	private List<Token> blockStructure;
	private boolean simulation;
	private boolean wrapDisabled;

	private Token potentialWrapToken;
	private int counterIfWrapped;
	private int lineCounter;

	public CommentWrapExecutor(TokenManager tokenManager, DefaultCodeFormatterOptions options) {
		this.tm = tokenManager;
		this.options = options;
	}

	/**
	 * @param commentToken token to wrap
	 * @param startPosition position in line of the beginning of the comment
	 * @param simulate if {@code true}, the properties of internal tokens will not really change. This
	 * mode is useful for checking how much space the comment takes.
	 * @param noWrap if {@code true}, it means that wrapping is disabled for this comment (for example because there's
	 * a NON-NLS tag after it). This method is still useful for checking comment length in that case.
	 * @return position in line at the end of comment
	 */
	public int wrapMultiLineComment(Token commentToken, int startPosition, boolean simulate, boolean noWrap) {
		this.lineCounter = 1;
		this.counter = startPosition;

		List<Token> structure = commentToken.getInternalStructure();
		if (structure == null || structure.isEmpty())
			return startPosition + this.tm.getLength(commentToken, startPosition);

		commentToken.setIndent(this.tm.toIndent(startPosition, true));
		this.lineStartPosition = commentToken.getIndent();
		this.simulation = simulate;
		this.wrapDisabled = noWrap;
		this.potentialWrapToken = null;
		this.blockStructure = structure;
		traverse(structure, 0);

		boolean newLinesAtBoundries = commentToken.tokenType == TokenNameCOMMENT_JAVADOC
				? this.options.comment_new_lines_at_javadoc_boundaries
				: this.options.comment_new_lines_at_block_boundaries;
		if (this.lineCounter > 1 && newLinesAtBoundries) {
			Token endingToken = structure.get(structure.size() - 1);
			if (!simulate && endingToken.tokenType != TokenNameNotAToken) {
				structure.get(0).breakAfter();
				endingToken.breakBefore();
				endingToken.setAlign(1);
			}
			return startPosition + this.tm.getLength(endingToken, startPosition);
		} else if (this.lineCounter > 1 && !newLinesAtBoundries) {
			// the rest of this code assumes that newLinesAtBoundries==true, so now substract the additional lines
			this.lineCounter -= 2;
		}
		return this.counter;
	}

	public int getLinesCount() {
		return this.lineCounter;
	}

	@Override
	protected boolean token(Token token, int index) {
		int positionIfNewLine = this.lineStartPosition + token.getAlign() + token.getIndent();
		if (token.tokenType != TokenNameNotAToken)
			positionIfNewLine += COMMENT_LINE_SEPARATOR_LENGTH;
		if (getLineBreaksBefore() > 0) {
			this.lineCounter = Math.max(this.lineCounter + getLineBreaksBefore(), 4);
			this.counter = positionIfNewLine;
			this.potentialWrapToken = null;

			if (token.getWrapPolicy() == null && token.getAlign() == 0) {
				// Indents are reserved for code inside <pre>.
				// Indentation of javadoc tags can be achieved with align
				token.setAlign(token.getIndent());
				token.setIndent(0);
			}
		}

		boolean canWrap = token.getWrapPolicy() == null && getNext() != null
				&& getLineBreaksBefore() == 0 && getLineBreaksBefore() == 0 && index > 1
				&& positionIfNewLine < this.counter;
		if (canWrap) {
			this.potentialWrapToken = token;
			this.counterIfWrapped = positionIfNewLine;
		}

		this.counter += this.tm.getLength(token, this.counter);
		this.counterIfWrapped += this.tm.getLength(token, this.counterIfWrapped);
		if (shouldWrap()) {
			this.counter = this.counterIfWrapped;
			if (!this.simulation) {
				this.potentialWrapToken.breakBefore();
				// Indents are reserved for code inside <pre>.
				// Indentation of javadoc tags can be achieved with align
				this.potentialWrapToken.setAlign(this.potentialWrapToken.getIndent());
				this.potentialWrapToken.setIndent(0);
			}
			this.lineCounter = Math.max(this.lineCounter + 1, 4);
			this.potentialWrapToken = null;
		}

		if (isSpaceAfter()) {
			this.counter++;
			this.counterIfWrapped++;
		}

		return true;
	}

	private boolean shouldWrap() {
		if (this.wrapDisabled || this.counter <= this.options.comment_line_length)
			return false;
		if (this.potentialWrapToken == null) {
			boolean isFormattingEnabled = this.blockStructure.get(0).getWrapPolicy() == null;
			if (isFormattingEnabled) {
				this.lineCounter = Math.max(this.lineCounter, 3);
			}
			return false;
		}

		if (this.options.comment_new_lines_at_javadoc_boundaries) {
			if (getNext() == null) { // the closing token will go to the next line anyway
				this.lineCounter = Math.max(this.lineCounter, 3);
				return false;
			}
			if (this.lineCounter == 1) {
				// when wrapping the first line of javadoc (more asterisks in opening token), the line will
				// move to the left so it may not need wrapping in the end
				int openingTokenLength = this.tm.getLength(this.blockStructure.get(0), 0);
				if (this.counter - (openingTokenLength - 2) <= this.options.comment_line_length) {
					this.counter -= (openingTokenLength - 2);
					this.lineCounter = Math.max(this.lineCounter, 3);
					return false;
				}
			}
		}
		return true;
	}

	public void wrapLineComment(Token commentToken, int startPosition) {
		List<Token> structure = commentToken.getInternalStructure();
		if (structure == null || structure.isEmpty())
			return;
		int commentIndex = this.tm.indexOf(commentToken);
		boolean isHeader = this.tm.isInHeader(commentIndex);
		boolean formattingEnabled = (this.options.comment_format_line_comment && !isHeader)
				|| (this.options.comment_format_header && isHeader);
		if (!formattingEnabled)
			return;

		int position = startPosition;
		startPosition = this.tm.toIndent(startPosition, true);
		int indent = startPosition;

		for (Token token : structure) {
			if (token.hasNLSTag()) {
				this.nlsTags.add(token);
				position += token.countChars() + (token.isSpaceBefore() ? 1 : 0);
			}
		}

		Token whitespace = null;
		Token prefix = structure.get(0);
		if (prefix.tokenType == TokenNameWHITESPACE) {
			whitespace = new Token(prefix);
			whitespace.breakBefore();
			whitespace.setWrapPolicy(new WrapPolicy(0, commentIndex, false));
			prefix = structure.get(1);
		}
		int prefixEnd = commentToken.originalStart + 1;
		if (prefix.tokenType == TokenNameCOMMENT_LINE)
			prefixEnd = Math.max(prefixEnd, prefix.originalEnd);
		prefix = new Token(commentToken.originalStart, prefixEnd, TokenNameCOMMENT_LINE);
		if (whitespace == null) {
			prefix.breakBefore();
			prefix.setWrapPolicy(new WrapPolicy(0, commentIndex, false));
		}

		int lineStartIndex = whitespace == null ? 0 : 1;
		for (int i = 0; i < structure.size(); i++) {
			Token token = structure.get(i);
			token.setIndent(indent);
			if (token.hasNLSTag()) {
				this.nlsTags.remove(token);
				continue;
			}
			if (token.isSpaceBefore())
				position++;
			if (token.getLineBreaksBefore() > 0) {
				position = startPosition;
				lineStartIndex = whitespace == null ? i : i + 1;
				if (whitespace != null && token != whitespace) {
					token.clearLineBreaksBefore();
					structure.add(i, whitespace);
					token = whitespace;
				}
			}
			position += this.tm.getLength(token, position);
			if (position > this.options.comment_line_length && i > lineStartIndex + 1) {
				structure.add(i, prefix);
				if (whitespace != null)
					structure.add(i, whitespace);

				structure.removeAll(this.nlsTags);
				structure.addAll(i, this.nlsTags);
				i = i + this.nlsTags.size() - 1;
				this.nlsTags.clear();
			}
		}
		this.nlsTags.clear();
	}
}
