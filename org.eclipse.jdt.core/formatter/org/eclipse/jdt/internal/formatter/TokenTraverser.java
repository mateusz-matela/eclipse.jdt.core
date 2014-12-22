package org.eclipse.jdt.internal.formatter;

import java.util.List;

public abstract class TokenTraverser {
		/** General purpose field that can be used by subclasses to count things */
		protected int counter = 0;
		/** General purpose field that can be used by subclasses to store an integer value */
		protected int value = 0;

		private boolean spaceBefore, spaceAfter;
		private int lineBreaksBefore, lineBreaksAfter;
		private Token previous, current, next;
		private boolean structureChanged = false;

		protected abstract boolean token(Token token, int index);

		protected void structureChanged() {
			this.structureChanged = true;
		}
	
		protected boolean isSpaceBefore() {
			return this.spaceBefore;
		}
	
		protected boolean isSpaceAfter() {
			return this.spaceAfter;
		}
	
		protected int getLineBreaksBefore() {
			return this.lineBreaksBefore;
		}
	
		protected int getLineBreaksAfter() {
			return this.lineBreaksAfter;
		}
	
		protected Token getPrevious() {
			return this.previous;
		}

		protected Token getCurrent() {
			return this.current;
		}

		protected Token getNext() {
			return this.next;
		}

		private void initTraverse(List<Token> tokens, int startIndex) {
			if (tokens.isEmpty())
				return;
			this.structureChanged = false;

			this.previous = this.next = null;
			if (startIndex > 0)
				this.previous = tokens.get(startIndex - 1);
			this.current = tokens.get(startIndex);
			this.lineBreaksBefore = Math.max(this.previous != null ? this.previous.getLineBreaksAfter() : 0,
					this.current.getLineBreaksBefore());
			this.spaceBefore = this.current.isSpaceBefore();
			if (this.lineBreaksBefore == 0) {
				this.spaceBefore = this.spaceBefore || (this.previous != null && this.previous.isSpaceAfter());
			}
		}

		public int traverse(List<Token> tokens, int startIndex) {
			initTraverse(tokens, startIndex);

			for (int i = startIndex; i < tokens.size(); i++) {
				if (this.structureChanged)
					initTraverse(tokens, i);

				this.next = null;
				if (i < tokens.size() - 1) {
					this.next = tokens.get(i + 1);
				}
				this.lineBreaksAfter = Math.max(this.current.getLineBreaksAfter(),
						this.next != null ? this.next.getLineBreaksBefore() : 0);
				this.spaceAfter = this.current.isSpaceAfter();
				if (this.lineBreaksAfter == 0) {
					this.spaceAfter = this.spaceAfter || (this.next != null && this.next.isSpaceBefore());
				}

				if (!this.token(this.current, i))
					return i;

				if (this.next != null) {
					this.previous = this.current;
					this.current = this.next;
					this.lineBreaksBefore = this.lineBreaksAfter;
					this.spaceBefore = this.spaceAfter;
					if (this.lineBreaksBefore > 0)
						this.spaceBefore = this.current.isSpaceBefore();
				}
			}
			return tokens.size() - 1;
		}
	}