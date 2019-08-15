//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//     ANTLR Version: 4.7.2
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

// Generated from /Users/azuri_hirohinata/Desktop/ANTLR/calc/Expression.g4 by ANTLR 4.7.2

// Unreachable code detected
#pragma warning disable 0162
// The variable '...' is assigned but its value is never used
#pragma warning disable 0219
// Missing XML comment for publicly visible type or member '...'
#pragma warning disable 1591
// Ambiguous reference in cref attribute
#pragma warning disable 419

using System;
using System.IO;
using System.Text;
using System.Diagnostics;
using System.Collections.Generic;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using DFA = Antlr4.Runtime.Dfa.DFA;

[System.CodeDom.Compiler.GeneratedCode("ANTLR", "4.7.2")]
[System.CLSCompliant(false)]
public partial class ExpressionParser : Parser {
	protected static DFA[] decisionToDFA;
	protected static PredictionContextCache sharedContextCache = new PredictionContextCache();
	public const int
		PLUS=1, MINUS=2, ASTERISK=3, SLASH=4, HAT=5, OPEN_PAREN=6, CLOSE_PAREN=7, 
		COMMA=8, UINT=9, IDENTIFIER=10, WS=11;
	public const int
		RULE_input = 0, RULE_expr = 1, RULE_paren_expr = 2, RULE_num = 3;
	public static readonly string[] ruleNames = {
		"input", "expr", "paren_expr", "num"
	};

	private static readonly string[] _LiteralNames = {
		null, "'+'", "'-'", "'*'", "'/'", "'^'", "'('", "')'", "','"
	};
	private static readonly string[] _SymbolicNames = {
		null, "PLUS", "MINUS", "ASTERISK", "SLASH", "HAT", "OPEN_PAREN", "CLOSE_PAREN", 
		"COMMA", "UINT", "IDENTIFIER", "WS"
	};
	public static readonly IVocabulary DefaultVocabulary = new Vocabulary(_LiteralNames, _SymbolicNames);

	[NotNull]
	public override IVocabulary Vocabulary
	{
		get
		{
			return DefaultVocabulary;
		}
	}

	public override string GrammarFileName { get { return "Expression.g4"; } }

	public override string[] RuleNames { get { return ruleNames; } }

	public override string SerializedAtn { get { return new string(_serializedATN); } }

	static ExpressionParser() {
		decisionToDFA = new DFA[_ATN.NumberOfDecisions];
		for (int i = 0; i < _ATN.NumberOfDecisions; i++) {
			decisionToDFA[i] = new DFA(_ATN.GetDecisionState(i), i);
		}
	}

		public ExpressionParser(ITokenStream input) : this(input, Console.Out, Console.Error) { }

		public ExpressionParser(ITokenStream input, TextWriter output, TextWriter errorOutput)
		: base(input, output, errorOutput)
	{
		Interpreter = new ParserATNSimulator(this, _ATN, decisionToDFA, sharedContextCache);
	}

	public partial class InputContext : ParserRuleContext {
		public ExprContext expr() {
			return GetRuleContext<ExprContext>(0);
		}
		public ITerminalNode Eof() { return GetToken(ExpressionParser.Eof, 0); }
		public InputContext(ParserRuleContext parent, int invokingState)
			: base(parent, invokingState)
		{
		}
		public override int RuleIndex { get { return RULE_input; } }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitInput(this);
			else return visitor.VisitChildren(this);
		}
	}

	[RuleVersion(0)]
	public InputContext input() {
		InputContext _localctx = new InputContext(Context, State);
		EnterRule(_localctx, 0, RULE_input);
		try {
			EnterOuterAlt(_localctx, 1);
			{
			State = 8; expr(0);
			State = 9; Match(Eof);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			ErrorHandler.ReportError(this, re);
			ErrorHandler.Recover(this, re);
		}
		finally {
			ExitRule();
		}
		return _localctx;
	}

	public partial class ExprContext : ParserRuleContext {
		public ExprContext(ParserRuleContext parent, int invokingState)
			: base(parent, invokingState)
		{
		}
		public override int RuleIndex { get { return RULE_expr; } }
	 
		public ExprContext() { }
		public virtual void CopyFrom(ExprContext context) {
			base.CopyFrom(context);
		}
	}
	public partial class Expr_additiveContext : ExprContext {
		public ExprContext lhs;
		public IToken op;
		public ExprContext rhs;
		public ExprContext[] expr() {
			return GetRuleContexts<ExprContext>();
		}
		public ExprContext expr(int i) {
			return GetRuleContext<ExprContext>(i);
		}
		public ITerminalNode PLUS() { return GetToken(ExpressionParser.PLUS, 0); }
		public ITerminalNode MINUS() { return GetToken(ExpressionParser.MINUS, 0); }
		public Expr_additiveContext(ExprContext context) { CopyFrom(context); }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitExpr_additive(this);
			else return visitor.VisitChildren(this);
		}
	}
	public partial class Expr_powerContext : ExprContext {
		public ExprContext lhs;
		public ExprContext rhs;
		public ITerminalNode HAT() { return GetToken(ExpressionParser.HAT, 0); }
		public ExprContext[] expr() {
			return GetRuleContexts<ExprContext>();
		}
		public ExprContext expr(int i) {
			return GetRuleContext<ExprContext>(i);
		}
		public Expr_powerContext(ExprContext context) { CopyFrom(context); }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitExpr_power(this);
			else return visitor.VisitChildren(this);
		}
	}
	public partial class Expr_funccallContext : ExprContext {
		public IToken funcname;
		public ExprContext _expr;
		public IList<ExprContext> _args = new List<ExprContext>();
		public ITerminalNode OPEN_PAREN() { return GetToken(ExpressionParser.OPEN_PAREN, 0); }
		public ITerminalNode CLOSE_PAREN() { return GetToken(ExpressionParser.CLOSE_PAREN, 0); }
		public ITerminalNode IDENTIFIER() { return GetToken(ExpressionParser.IDENTIFIER, 0); }
		public ExprContext[] expr() {
			return GetRuleContexts<ExprContext>();
		}
		public ExprContext expr(int i) {
			return GetRuleContext<ExprContext>(i);
		}
		public ITerminalNode[] COMMA() { return GetTokens(ExpressionParser.COMMA); }
		public ITerminalNode COMMA(int i) {
			return GetToken(ExpressionParser.COMMA, i);
		}
		public Expr_funccallContext(ExprContext context) { CopyFrom(context); }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitExpr_funccall(this);
			else return visitor.VisitChildren(this);
		}
	}
	public partial class Expr_multipricativeContext : ExprContext {
		public ExprContext lhs;
		public IToken op;
		public ExprContext rhs;
		public ExprContext[] expr() {
			return GetRuleContexts<ExprContext>();
		}
		public ExprContext expr(int i) {
			return GetRuleContext<ExprContext>(i);
		}
		public ITerminalNode ASTERISK() { return GetToken(ExpressionParser.ASTERISK, 0); }
		public ITerminalNode SLASH() { return GetToken(ExpressionParser.SLASH, 0); }
		public Paren_exprContext paren_expr() {
			return GetRuleContext<Paren_exprContext>(0);
		}
		public Expr_multipricativeContext(ExprContext context) { CopyFrom(context); }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitExpr_multipricative(this);
			else return visitor.VisitChildren(this);
		}
	}
	public partial class Expr_unaryContext : ExprContext {
		public IToken op;
		public ExprContext expr() {
			return GetRuleContext<ExprContext>(0);
		}
		public ITerminalNode PLUS() { return GetToken(ExpressionParser.PLUS, 0); }
		public ITerminalNode MINUS() { return GetToken(ExpressionParser.MINUS, 0); }
		public Expr_unaryContext(ExprContext context) { CopyFrom(context); }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitExpr_unary(this);
			else return visitor.VisitChildren(this);
		}
	}
	public partial class Expr_noneContext : ExprContext {
		public NumContext num() {
			return GetRuleContext<NumContext>(0);
		}
		public Paren_exprContext paren_expr() {
			return GetRuleContext<Paren_exprContext>(0);
		}
		public Expr_noneContext(ExprContext context) { CopyFrom(context); }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitExpr_none(this);
			else return visitor.VisitChildren(this);
		}
	}

	[RuleVersion(0)]
	public ExprContext expr() {
		return expr(0);
	}

	private ExprContext expr(int _p) {
		ParserRuleContext _parentctx = Context;
		int _parentState = State;
		ExprContext _localctx = new ExprContext(Context, _parentState);
		ExprContext _prevctx = _localctx;
		int _startState = 2;
		EnterRecursionRule(_localctx, 2, RULE_expr, _p);
		int _la;
		try {
			int _alt;
			EnterOuterAlt(_localctx, 1);
			{
			State = 29;
			ErrorHandler.Sync(this);
			switch (TokenStream.LA(1)) {
			case UINT:
				{
				_localctx = new Expr_noneContext(_localctx);
				Context = _localctx;
				_prevctx = _localctx;

				State = 12; num();
				}
				break;
			case OPEN_PAREN:
				{
				_localctx = new Expr_noneContext(_localctx);
				Context = _localctx;
				_prevctx = _localctx;
				State = 13; paren_expr();
				}
				break;
			case PLUS:
			case MINUS:
				{
				_localctx = new Expr_unaryContext(_localctx);
				Context = _localctx;
				_prevctx = _localctx;
				State = 14;
				((Expr_unaryContext)_localctx).op = TokenStream.LT(1);
				_la = TokenStream.LA(1);
				if ( !(_la==PLUS || _la==MINUS) ) {
					((Expr_unaryContext)_localctx).op = ErrorHandler.RecoverInline(this);
				}
				else {
					ErrorHandler.ReportMatch(this);
				    Consume();
				}
				State = 15; expr(6);
				}
				break;
			case IDENTIFIER:
				{
				_localctx = new Expr_funccallContext(_localctx);
				Context = _localctx;
				_prevctx = _localctx;
				State = 16; ((Expr_funccallContext)_localctx).funcname = Match(IDENTIFIER);
				State = 17; Match(OPEN_PAREN);
				State = 26;
				ErrorHandler.Sync(this);
				_la = TokenStream.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << PLUS) | (1L << MINUS) | (1L << OPEN_PAREN) | (1L << UINT) | (1L << IDENTIFIER))) != 0)) {
					{
					State = 18; ((Expr_funccallContext)_localctx)._expr = expr(0);
					((Expr_funccallContext)_localctx)._args.Add(((Expr_funccallContext)_localctx)._expr);
					State = 23;
					ErrorHandler.Sync(this);
					_la = TokenStream.LA(1);
					while (_la==COMMA) {
						{
						{
						State = 19; Match(COMMA);
						State = 20; ((Expr_funccallContext)_localctx)._expr = expr(0);
						((Expr_funccallContext)_localctx)._args.Add(((Expr_funccallContext)_localctx)._expr);
						}
						}
						State = 25;
						ErrorHandler.Sync(this);
						_la = TokenStream.LA(1);
					}
					}
				}

				State = 28; Match(CLOSE_PAREN);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			Context.Stop = TokenStream.LT(-1);
			State = 44;
			ErrorHandler.Sync(this);
			_alt = Interpreter.AdaptivePredict(TokenStream,4,Context);
			while ( _alt!=2 && _alt!=global::Antlr4.Runtime.Atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					if ( ParseListeners!=null )
						TriggerExitRuleEvent();
					_prevctx = _localctx;
					{
					State = 42;
					ErrorHandler.Sync(this);
					switch ( Interpreter.AdaptivePredict(TokenStream,3,Context) ) {
					case 1:
						{
						_localctx = new Expr_powerContext(new ExprContext(_parentctx, _parentState));
						((Expr_powerContext)_localctx).lhs = _prevctx;
						PushNewRecursionContext(_localctx, _startState, RULE_expr);
						State = 31;
						if (!(Precpred(Context, 5))) throw new FailedPredicateException(this, "Precpred(Context, 5)");
						State = 32; Match(HAT);
						State = 33; ((Expr_powerContext)_localctx).rhs = expr(5);
						}
						break;
					case 2:
						{
						_localctx = new Expr_multipricativeContext(new ExprContext(_parentctx, _parentState));
						((Expr_multipricativeContext)_localctx).lhs = _prevctx;
						PushNewRecursionContext(_localctx, _startState, RULE_expr);
						State = 34;
						if (!(Precpred(Context, 4))) throw new FailedPredicateException(this, "Precpred(Context, 4)");
						State = 35;
						((Expr_multipricativeContext)_localctx).op = TokenStream.LT(1);
						_la = TokenStream.LA(1);
						if ( !(_la==ASTERISK || _la==SLASH) ) {
							((Expr_multipricativeContext)_localctx).op = ErrorHandler.RecoverInline(this);
						}
						else {
							ErrorHandler.ReportMatch(this);
						    Consume();
						}
						State = 36; ((Expr_multipricativeContext)_localctx).rhs = expr(5);
						}
						break;
					case 3:
						{
						_localctx = new Expr_additiveContext(new ExprContext(_parentctx, _parentState));
						((Expr_additiveContext)_localctx).lhs = _prevctx;
						PushNewRecursionContext(_localctx, _startState, RULE_expr);
						State = 37;
						if (!(Precpred(Context, 2))) throw new FailedPredicateException(this, "Precpred(Context, 2)");
						State = 38;
						((Expr_additiveContext)_localctx).op = TokenStream.LT(1);
						_la = TokenStream.LA(1);
						if ( !(_la==PLUS || _la==MINUS) ) {
							((Expr_additiveContext)_localctx).op = ErrorHandler.RecoverInline(this);
						}
						else {
							ErrorHandler.ReportMatch(this);
						    Consume();
						}
						State = 39; ((Expr_additiveContext)_localctx).rhs = expr(3);
						}
						break;
					case 4:
						{
						_localctx = new Expr_multipricativeContext(new ExprContext(_parentctx, _parentState));
						PushNewRecursionContext(_localctx, _startState, RULE_expr);
						State = 40;
						if (!(Precpred(Context, 3))) throw new FailedPredicateException(this, "Precpred(Context, 3)");
						State = 41; paren_expr();
						}
						break;
					}
					} 
				}
				State = 46;
				ErrorHandler.Sync(this);
				_alt = Interpreter.AdaptivePredict(TokenStream,4,Context);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			ErrorHandler.ReportError(this, re);
			ErrorHandler.Recover(this, re);
		}
		finally {
			UnrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public partial class Paren_exprContext : ParserRuleContext {
		public ITerminalNode OPEN_PAREN() { return GetToken(ExpressionParser.OPEN_PAREN, 0); }
		public ExprContext expr() {
			return GetRuleContext<ExprContext>(0);
		}
		public ITerminalNode CLOSE_PAREN() { return GetToken(ExpressionParser.CLOSE_PAREN, 0); }
		public Paren_exprContext(ParserRuleContext parent, int invokingState)
			: base(parent, invokingState)
		{
		}
		public override int RuleIndex { get { return RULE_paren_expr; } }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitParen_expr(this);
			else return visitor.VisitChildren(this);
		}
	}

	[RuleVersion(0)]
	public Paren_exprContext paren_expr() {
		Paren_exprContext _localctx = new Paren_exprContext(Context, State);
		EnterRule(_localctx, 4, RULE_paren_expr);
		try {
			EnterOuterAlt(_localctx, 1);
			{
			State = 47; Match(OPEN_PAREN);
			State = 48; expr(0);
			State = 49; Match(CLOSE_PAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			ErrorHandler.ReportError(this, re);
			ErrorHandler.Recover(this, re);
		}
		finally {
			ExitRule();
		}
		return _localctx;
	}

	public partial class NumContext : ParserRuleContext {
		public ITerminalNode UINT() { return GetToken(ExpressionParser.UINT, 0); }
		public NumContext(ParserRuleContext parent, int invokingState)
			: base(parent, invokingState)
		{
		}
		public override int RuleIndex { get { return RULE_num; } }
		public override TResult Accept<TResult>(IParseTreeVisitor<TResult> visitor) {
			IExpressionVisitor<TResult> typedVisitor = visitor as IExpressionVisitor<TResult>;
			if (typedVisitor != null) return typedVisitor.VisitNum(this);
			else return visitor.VisitChildren(this);
		}
	}

	[RuleVersion(0)]
	public NumContext num() {
		NumContext _localctx = new NumContext(Context, State);
		EnterRule(_localctx, 6, RULE_num);
		try {
			EnterOuterAlt(_localctx, 1);
			{
			State = 51; Match(UINT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			ErrorHandler.ReportError(this, re);
			ErrorHandler.Recover(this, re);
		}
		finally {
			ExitRule();
		}
		return _localctx;
	}

	public override bool Sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 1: return expr_sempred((ExprContext)_localctx, predIndex);
		}
		return true;
	}
	private bool expr_sempred(ExprContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0: return Precpred(Context, 5);
		case 1: return Precpred(Context, 4);
		case 2: return Precpred(Context, 2);
		case 3: return Precpred(Context, 3);
		}
		return true;
	}

	private static char[] _serializedATN = {
		'\x3', '\x608B', '\xA72A', '\x8133', '\xB9ED', '\x417C', '\x3BE7', '\x7786', 
		'\x5964', '\x3', '\r', '\x38', '\x4', '\x2', '\t', '\x2', '\x4', '\x3', 
		'\t', '\x3', '\x4', '\x4', '\t', '\x4', '\x4', '\x5', '\t', '\x5', '\x3', 
		'\x2', '\x3', '\x2', '\x3', '\x2', '\x3', '\x3', '\x3', '\x3', '\x3', 
		'\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', 
		'\x3', '\x3', '\x3', '\x3', '\x3', '\a', '\x3', '\x18', '\n', '\x3', '\f', 
		'\x3', '\xE', '\x3', '\x1B', '\v', '\x3', '\x5', '\x3', '\x1D', '\n', 
		'\x3', '\x3', '\x3', '\x5', '\x3', ' ', '\n', '\x3', '\x3', '\x3', '\x3', 
		'\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', 
		'\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\x3', '\a', '\x3', 
		'-', '\n', '\x3', '\f', '\x3', '\xE', '\x3', '\x30', '\v', '\x3', '\x3', 
		'\x4', '\x3', '\x4', '\x3', '\x4', '\x3', '\x4', '\x3', '\x5', '\x3', 
		'\x5', '\x3', '\x5', '\x2', '\x3', '\x4', '\x6', '\x2', '\x4', '\x6', 
		'\b', '\x2', '\x4', '\x3', '\x2', '\x3', '\x4', '\x3', '\x2', '\x5', '\x6', 
		'\x2', '<', '\x2', '\n', '\x3', '\x2', '\x2', '\x2', '\x4', '\x1F', '\x3', 
		'\x2', '\x2', '\x2', '\x6', '\x31', '\x3', '\x2', '\x2', '\x2', '\b', 
		'\x35', '\x3', '\x2', '\x2', '\x2', '\n', '\v', '\x5', '\x4', '\x3', '\x2', 
		'\v', '\f', '\a', '\x2', '\x2', '\x3', '\f', '\x3', '\x3', '\x2', '\x2', 
		'\x2', '\r', '\xE', '\b', '\x3', '\x1', '\x2', '\xE', ' ', '\x5', '\b', 
		'\x5', '\x2', '\xF', ' ', '\x5', '\x6', '\x4', '\x2', '\x10', '\x11', 
		'\t', '\x2', '\x2', '\x2', '\x11', ' ', '\x5', '\x4', '\x3', '\b', '\x12', 
		'\x13', '\a', '\f', '\x2', '\x2', '\x13', '\x1C', '\a', '\b', '\x2', '\x2', 
		'\x14', '\x19', '\x5', '\x4', '\x3', '\x2', '\x15', '\x16', '\a', '\n', 
		'\x2', '\x2', '\x16', '\x18', '\x5', '\x4', '\x3', '\x2', '\x17', '\x15', 
		'\x3', '\x2', '\x2', '\x2', '\x18', '\x1B', '\x3', '\x2', '\x2', '\x2', 
		'\x19', '\x17', '\x3', '\x2', '\x2', '\x2', '\x19', '\x1A', '\x3', '\x2', 
		'\x2', '\x2', '\x1A', '\x1D', '\x3', '\x2', '\x2', '\x2', '\x1B', '\x19', 
		'\x3', '\x2', '\x2', '\x2', '\x1C', '\x14', '\x3', '\x2', '\x2', '\x2', 
		'\x1C', '\x1D', '\x3', '\x2', '\x2', '\x2', '\x1D', '\x1E', '\x3', '\x2', 
		'\x2', '\x2', '\x1E', ' ', '\a', '\t', '\x2', '\x2', '\x1F', '\r', '\x3', 
		'\x2', '\x2', '\x2', '\x1F', '\xF', '\x3', '\x2', '\x2', '\x2', '\x1F', 
		'\x10', '\x3', '\x2', '\x2', '\x2', '\x1F', '\x12', '\x3', '\x2', '\x2', 
		'\x2', ' ', '.', '\x3', '\x2', '\x2', '\x2', '!', '\"', '\f', '\a', '\x2', 
		'\x2', '\"', '#', '\a', '\a', '\x2', '\x2', '#', '-', '\x5', '\x4', '\x3', 
		'\a', '$', '%', '\f', '\x6', '\x2', '\x2', '%', '&', '\t', '\x3', '\x2', 
		'\x2', '&', '-', '\x5', '\x4', '\x3', '\a', '\'', '(', '\f', '\x4', '\x2', 
		'\x2', '(', ')', '\t', '\x2', '\x2', '\x2', ')', '-', '\x5', '\x4', '\x3', 
		'\x5', '*', '+', '\f', '\x5', '\x2', '\x2', '+', '-', '\x5', '\x6', '\x4', 
		'\x2', ',', '!', '\x3', '\x2', '\x2', '\x2', ',', '$', '\x3', '\x2', '\x2', 
		'\x2', ',', '\'', '\x3', '\x2', '\x2', '\x2', ',', '*', '\x3', '\x2', 
		'\x2', '\x2', '-', '\x30', '\x3', '\x2', '\x2', '\x2', '.', ',', '\x3', 
		'\x2', '\x2', '\x2', '.', '/', '\x3', '\x2', '\x2', '\x2', '/', '\x5', 
		'\x3', '\x2', '\x2', '\x2', '\x30', '.', '\x3', '\x2', '\x2', '\x2', '\x31', 
		'\x32', '\a', '\b', '\x2', '\x2', '\x32', '\x33', '\x5', '\x4', '\x3', 
		'\x2', '\x33', '\x34', '\a', '\t', '\x2', '\x2', '\x34', '\a', '\x3', 
		'\x2', '\x2', '\x2', '\x35', '\x36', '\a', '\v', '\x2', '\x2', '\x36', 
		'\t', '\x3', '\x2', '\x2', '\x2', '\a', '\x19', '\x1C', '\x1F', ',', '.',
	};

	public static readonly ATN _ATN =
		new ATNDeserializer().Deserialize(_serializedATN);


}
