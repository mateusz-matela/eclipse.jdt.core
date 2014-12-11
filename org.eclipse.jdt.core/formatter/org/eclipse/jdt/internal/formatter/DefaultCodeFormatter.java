/*******************************************************************************
 * Copyright (c) 2000, 2013 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Jesper Steen Moller - Contributions for
 *								bug 404146 - [1.7][compiler] nested try-catch-finally-blocks leads to unrunnable Java byte code
 *******************************************************************************/
package org.eclipse.jdt.internal.formatter;

import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameEOF;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameNotAToken;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_JAVADOC;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_BLOCK;
import static org.eclipse.jdt.internal.compiler.parser.TerminalTokens.TokenNameCOMMENT_LINE;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.compiler.IProblem;
import org.eclipse.jdt.core.compiler.InvalidInputException;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.Comment;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.Javadoc;
import org.eclipse.jdt.core.dom.TypeDeclaration;
import org.eclipse.jdt.core.formatter.CodeFormatter;
import org.eclipse.jdt.core.formatter.DefaultCodeFormatterConstants;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.parser.Scanner;
import org.eclipse.jdt.internal.compiler.util.Util;
import org.eclipse.jdt.internal.formatter.linewrap.CommentWrapExecutor;
import org.eclipse.jdt.internal.formatter.redesign.CommentsPreparator;
import org.eclipse.jdt.internal.formatter.redesign.LineBreaksPreparator;
import org.eclipse.jdt.internal.formatter.redesign.SpacePreparator;
import org.eclipse.jdt.internal.formatter.redesign.TextEditsBuilder;
import org.eclipse.jdt.internal.formatter.redesign.Token;
import org.eclipse.jdt.internal.formatter.redesign.TokenManager;
import org.eclipse.jdt.internal.formatter.redesign.WrapPreparator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.TextEdit;

@SuppressWarnings({ "rawtypes" })
public class DefaultCodeFormatter extends CodeFormatter {

	/**
	 * Debug trace
	 */
	public static boolean DEBUG = false;

	private static final int K_COMMENTS_MASK = K_SINGLE_LINE_COMMENT
		| K_MULTI_LINE_COMMENT
		| K_JAVA_DOC;

	// Mask for code formatter kinds
	private static final int K_MASK = K_UNKNOWN
		| K_EXPRESSION
		| K_STATEMENTS
		| K_CLASS_BODY_DECLARATIONS
		| K_COMPILATION_UNIT
		| K_COMMENTS_MASK;

	private final DefaultCodeFormatterOptions originalOptions;

	private DefaultCodeFormatterOptions workingOptions;

	private final boolean oldCommentFormatOption;
	private final String sourceLevel;

	private String sourceString;
	private char[] sourceArray;

	private ASTNode astRoot;
	private List<Token> tokens = new ArrayList<Token>();
	private TokenManager tokenManager;

	public DefaultCodeFormatter() {
		this(new DefaultCodeFormatterOptions(DefaultCodeFormatterConstants.getJavaConventionsSettings()), null);
	}

	public DefaultCodeFormatter(DefaultCodeFormatterOptions options) {
		this(options, null);
	}

	public DefaultCodeFormatter(Map options) {
		this(null, options);
	}

	public DefaultCodeFormatter(DefaultCodeFormatterOptions defaultCodeFormatterOptions, Map options) {
		if (options != null) {
			this.originalOptions = new DefaultCodeFormatterOptions(options);
			this.workingOptions = new DefaultCodeFormatterOptions(options);
			this.oldCommentFormatOption = getOldCommentFormatOption(options);
			String compilerSource = (String) options.get(CompilerOptions.OPTION_Source);
			this.sourceLevel = compilerSource != null ? compilerSource : CompilerOptions.VERSION_1_8;
		} else {
			Map settings = DefaultCodeFormatterConstants.getJavaConventionsSettings();
			this.originalOptions = new DefaultCodeFormatterOptions(settings);
			this.workingOptions = new DefaultCodeFormatterOptions(settings);
			this.oldCommentFormatOption = true;
			this.sourceLevel = CompilerOptions.VERSION_1_8;
		}
		if (defaultCodeFormatterOptions != null) {
			this.originalOptions.set(defaultCodeFormatterOptions.getMap());
			this.workingOptions.set(defaultCodeFormatterOptions.getMap());
		}
	}

	@Deprecated
	private boolean getOldCommentFormatOption(Map options) {
		Object oldOption = options.get(DefaultCodeFormatterConstants.FORMATTER_COMMENT_FORMAT);
		return oldOption == null || DefaultCodeFormatterConstants.TRUE.equals(oldOption);
	}

	public String createIndentationString(final int indentationLevel) {
		if (indentationLevel < 0) {
			throw new IllegalArgumentException();
		}

		StringBuilder sb = new StringBuilder();
		int indent = indentationLevel * this.originalOptions.indentation_size;
		TextEditsBuilder.appendIndentationString(sb, this.originalOptions.tab_char, this.originalOptions.tab_size,
				indent, 0);
		return sb.toString();
	}

	/**
	 * @see org.eclipse.jdt.core.formatter.CodeFormatter#format(int, java.lang.String, int, int, int, java.lang.String)
	 */
	public TextEdit format(int kind, String source, int offset, int length, int indentationLevel, String lineSeparator) {
		return format(kind, source, new IRegion[] {new Region(offset, length)}, indentationLevel, lineSeparator);
	}

	/**
	 * {@inheritDoc}
	 */
	public TextEdit format(int kind, String source, IRegion[] regions, int indentationLevel, String lineSeparator) {
		if (!regionsSatisfiesPreconditions(regions, source.length())) {
			throw new IllegalArgumentException();
		}

		updateWorkingOptions(indentationLevel, lineSeparator, kind);

		if ((kind & K_COMMENTS_MASK) != 0)
			return formatComments(source, kind & K_COMMENTS_MASK, regions);

		MultiTextEdit result = new MultiTextEdit();
		if (prepareFormattedCode(source, kind) == null)
			return result;

		TextEditsBuilder resultBuilder = new TextEditsBuilder(this.sourceString, regions, this.tokenManager,
				this.workingOptions);
		this.tokenManager.traverse(0, resultBuilder);
		for (TextEdit edit : resultBuilder.getEdits()) {
			result.addChild(edit);
		}
		return result;
	}

	private boolean init(String source) {
		//if (Token.source == null)
			Token.source = source;
		this.sourceString = source;
		this.sourceArray = source.toCharArray();
		this.tokens.clear();
		this.tokenManager = new TokenManager(this.tokens, source, this.workingOptions);
		
		tokenizeSource();
		return !this.tokens.isEmpty();
	}

	//TODO make package private when this class and CommentsPreparator are in the same package
	public List<Token> prepareFormattedCode(String source, int kind) {
		if (!init(source))
			return null;

		this.astRoot = parseSourceCode(kind);
		if (this.astRoot == null)
			return null;

		if (kind != CodeFormatter.K_UNKNOWN)
			findHeader();

		prepareSpaces();
		prepareLineBreaks();
		prepareComments();
		prepareWraps();

		this.tokenManager.applyFormatOff();

		return this.tokens;
	}

	private void findHeader() {
		if (this.astRoot instanceof CompilationUnit) {
			List<TypeDeclaration> types = ((CompilationUnit) this.astRoot).types();
			if (!types.isEmpty()) {
				int headerEndIndex = this.tokenManager.firstIndexIn(types.get(0), -1);
				this.tokenManager.setHeaderEndIndex(headerEndIndex);
			}
		}
	}

	private TextEdit formatComments(String source, int kind, IRegion[] regions) {
		MultiTextEdit result = new MultiTextEdit();
		if (!init(source))
			return result;

		CommentsPreparator commentsPreparator = new CommentsPreparator(this.tokenManager, this.workingOptions,
				this.sourceLevel);
		CommentWrapExecutor commentWrapper = new CommentWrapExecutor(this.tokenManager, this.workingOptions);
		switch (kind) {
			case K_JAVA_DOC:
				ASTParser parser = ASTParser.newParser(AST.JLS8);
				for (Token token : this.tokens) {
					if (token.tokenType == TokenNameCOMMENT_JAVADOC) {
						parser.setSourceRange(token.originalStart, token.countChars());
						CompilationUnit cu = (CompilationUnit) parseSourceCode(parser, ASTParser.K_COMPILATION_UNIT, true);
						Javadoc javadoc = (Javadoc) cu.getCommentList().get(0);
						javadoc.accept(commentsPreparator);
						int startPosition = this.tokenManager.findSourcePositionInLine(token.originalStart);
						commentWrapper.wrapMultiLineComment(token, startPosition, false);
					}
				}
				break;
			case K_MULTI_LINE_COMMENT:
				for (int i = 0; i < this.tokens.size(); i++) {
					Token token = this.tokens.get(i);
					if (token.tokenType == TokenNameCOMMENT_BLOCK) {
						commentsPreparator.handleBlockComment(i);
						int startPosition = this.tokenManager.findSourcePositionInLine(token.originalStart);
						commentWrapper.wrapMultiLineComment(token, startPosition, false);
					}
				}
				break;
			case K_SINGLE_LINE_COMMENT:
				for (int i = 0; i < this.tokens.size(); i++) {
					Token token = this.tokens.get(i);
					if (token.tokenType == TokenNameCOMMENT_LINE) {
						commentsPreparator.handleLineComment(i);
						if (i >= this.tokens.size() || this.tokens.get(i) != token) {
							// current token has been removed and merged with previous one
							i--;
							token = this.tokens.get(i);
						}
						int startPosition = this.tokenManager.findSourcePositionInLine(token.originalStart);
						commentWrapper.wrapLineComment(token, startPosition);
					}
				}
				break;
			default:
				throw new AssertionError(String.valueOf(kind));
		}

		this.tokenManager.applyFormatOff();

		TextEditsBuilder resultBuilder = new TextEditsBuilder(source, regions, this.tokenManager, this.workingOptions);
		resultBuilder.setAlignChar(DefaultCodeFormatterOptions.SPACE);
		for (Token token : this.tokens) {
			List<Token> structure = token.getInternalStructure();
			if (structure != null && !structure.isEmpty())
				resultBuilder.processComment(token);
		}
		
		for (TextEdit edit : resultBuilder.getEdits()) {
			result.addChild(edit);
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	private ASTNode parseSourceCode(int kind) {
		ASTParser parser = ASTParser.newParser(AST.JLS8);
		Map parserOptions = JavaCore.getOptions();
		parserOptions.put(CompilerOptions.OPTION_Source, this.sourceLevel);
		parser.setCompilerOptions(parserOptions);

		switch (kind & K_MASK) {
			case K_COMPILATION_UNIT:
				return parseSourceCode(parser, ASTParser.K_COMPILATION_UNIT, true);
			case K_CLASS_BODY_DECLARATIONS:
				return parseSourceCode(parser, ASTParser.K_CLASS_BODY_DECLARATIONS, true);
			case K_STATEMENTS:
				return parseSourceCode(parser, ASTParser.K_STATEMENTS, true);
			case K_EXPRESSION:
				return parseSourceCode(parser, ASTParser.K_EXPRESSION, true);
			case K_UNKNOWN:
				int[] parserModes = { ASTParser.K_COMPILATION_UNIT, ASTParser.K_EXPRESSION,
						ASTParser.K_CLASS_BODY_DECLARATIONS, ASTParser.K_STATEMENTS};
				for (int parserMode : parserModes) {
					ASTNode astNode = parseSourceCode(parser, parserMode, false);
					if (astNode != null)
						return astNode;
					parser.setCompilerOptions(parserOptions); // parser loses compiler options after every use
				}
				return null;
			default:
				throw new IllegalArgumentException();
		}
	}

	private ASTNode parseSourceCode(ASTParser parser, int parserMode, boolean ignoreErrors) {
		parser.setKind(parserMode);
		parser.setSource(this.sourceArray);
		ASTNode astNode = parser.createAST(null);
		if (ignoreErrors)
			return astNode;

		boolean hasErrors = false;
		CompilationUnit root = (CompilationUnit) astNode.getRoot();
		for (IProblem problem : root.getProblems()) {
			if (problem.isError()) {
				hasErrors = true;
				break;
			}
		}
		return hasErrors ? null : astNode;
	}

	private void tokenizeSource() {
		this.tokens.clear();
		Scanner scanner = new Scanner(true, false, false/* nls */, CompilerOptions.versionToJdkLevel(this.sourceLevel),
				null/* taskTags */, null/* taskPriorities */, false/* taskCaseSensitive */);
		scanner.setSource(this.sourceArray);
		while (true) {
			try {
				int tokenType = scanner.getNextToken();
				if (tokenType == TokenNameEOF)
					break;
				Token token = Token.fromCurrent(scanner, tokenType);
				this.tokens.add(token);
			} catch (InvalidInputException e) {
				Token token = Token.fromCurrent(scanner, TokenNameNotAToken);
				this.tokens.add(token);
			}
		}
	}

	private void prepareSpaces() {
		SpacePreparator spacePreparator = new SpacePreparator(this.tokenManager, this.workingOptions);
		this.astRoot.accept(spacePreparator);
		spacePreparator.finishUp();
	}

	private void prepareLineBreaks() {
		LineBreaksPreparator breaksPreparator = new LineBreaksPreparator(this.tokenManager, this.workingOptions);
		this.astRoot.accept(breaksPreparator);
		breaksPreparator.finishUp();
	}

	private void prepareComments() {
		CommentsPreparator commentsPreparator = new CommentsPreparator(this.tokenManager, this.workingOptions,
				this.sourceLevel);
		List<Comment> comments = ((CompilationUnit) this.astRoot.getRoot()).getCommentList();
		for (Comment comment : comments) {
			comment.accept(commentsPreparator);
		}
		commentsPreparator.finishUp();
	}

	private void prepareWraps() {
		WrapPreparator wrapPreparator = new WrapPreparator(this.tokenManager, this.workingOptions);
		this.astRoot.accept(wrapPreparator);
		wrapPreparator.finishUp();
	}

	/**
	 * True if
	 * 1. All regions are within maxLength
	 * 2. regions are sorted
	 * 3. regions are not overlapping
	 */
	private boolean regionsSatisfiesPreconditions(IRegion[] regions, int maxLength) {
		int regionsLength = regions == null ? 0 : regions.length;
		if (regionsLength == 0) {
			return false;
		}

		IRegion first = regions[0];
		if (first.getOffset() < 0 || first.getLength() < 0 || first.getOffset() + first.getLength() > maxLength) {
			return false;
		}

		int lastOffset = first.getOffset() + first.getLength() - 1;
		for (int i= 1; i < regionsLength; i++) {
			IRegion current = regions[i];
			if (lastOffset > current.getOffset()) {
				return false;
			}

			if (current.getOffset() < 0 || current.getLength() < 0 || current.getOffset() + current.getLength() > maxLength) {
				return false;
			}

			lastOffset = current.getOffset() + current.getLength() - 1;
		}

		return true;
	}

	private void updateWorkingOptions(int indentationLevel, String lineSeparator, int kind) {
		this.workingOptions.line_separator = lineSeparator != null ? lineSeparator
				: this.originalOptions.line_separator;
		if (this.workingOptions.line_separator == null)
			this.workingOptions.line_separator = Util.LINE_SEPARATOR;

		this.workingOptions.initial_indentation_level = indentationLevel;

		boolean includeComments = (kind & F_INCLUDE_COMMENTS) != 0 || kind == 0;
		this.workingOptions.comment_format_javadoc_comment = this.originalOptions.comment_format_javadoc_comment
				&& this.oldCommentFormatOption && (includeComments || (kind & K_MASK) == K_JAVA_DOC);
		this.workingOptions.comment_format_block_comment = this.originalOptions.comment_format_block_comment
				&& this.oldCommentFormatOption && (includeComments || (kind & K_MASK) == K_MULTI_LINE_COMMENT);
		this.workingOptions.comment_format_line_comment = this.originalOptions.comment_format_line_comment
				&& this.oldCommentFormatOption && (includeComments || (kind & K_MASK) == K_SINGLE_LINE_COMMENT);
	}
}
