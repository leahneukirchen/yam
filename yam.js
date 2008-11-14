load("jsparse/jsparse.js")
memoize = false                 // for debugging, less cluttered output

/*
function foldr(f, initial, seq) {
  for (var i=seq.length-1; i >= 0; i--) {
    initial = f(initial, seq[i])
  }
  return initial
}

function chainr(p, s) {
  return action(sequence(p, repeat0(sequence(s, p))),
                function(ast) {
                  return foldr(function(v, action) {
                    return action[0](v, action[1]) },
                               ast[0], ast[1]);
                });
}
*/

function joined(p) { return join_action(p, "") }

/* Allow optional seperator at the end.  */
function lossyList(p, s) {
  return action(optional(sequence(wlist(p, s), optional(s))),
    function(ast) { return ast[0] ? ast[0] : [] })
}

var Expr = function(state) { return Expr(state); }
var ExprNoOp = function(state) { return ExprNoOp(state); }
var MatchTerm = function(state) { return MatchTerm(state); }

var ReservedWord = choice("fn", "if", "in", "let")
var ReservedOperator = choice("|", "@", "->")

var IdentifierBeginning = choice(range("a", "z"), "_", "$")
var IdentifierPart = choice(range("a", "z"), range("A", "Z"),
                            range("0", "9"), "_", "-", "$")
var IdentifierEnd = choice(IdentifierPart, "'", "!", "?")

var IdentifierName = joined(
  sequence(IdentifierBeginning,
           joined(repeat0(IdentifierPart)),
           joined(repeat0(IdentifierEnd))))

var ConstructorBeginning = choice(range("A", "Z"))
var ConstructorPart = choice(range("a", "z"), range("A", "Z"),
                             range("0", "9"))
var ConstructorEnd = choice(ConstructorPart)

var ConstructorName = joined(
  sequence(ConstructorBeginning,
           joined(repeat0(ConstructorPart)),
           joined(repeat0(ConstructorEnd))))

var Constructor = action(sequence(ConstructorName, Expr),
  function(ast) { return { type: "cons", name: ast[0], expr: ast[1] } })

var Operator = butnot(join_action(repeat1(
  choice("#", "$", "%", "&", "*", "+", "-", ".", "/", ":",
         "<", "=", ">", "?", "@", "\\", "^", "|", "~")),
  ""), ReservedOperator)
var OperatorName = action(sequence(expect("("), Operator, expect(")")),
  function(ast) { return ast[0] })

var Identifier = choice(butnot(IdentifierName, ReservedWord),
                        OperatorName)

var Var = action(Identifier,
  function(ast) { return { type: "var", name: ast } })

/* adapted from es3.js by Chris Double.  */

var Zero = "0"
var DecimalDigit = range("0", "9")
var DecimalIntegerLiteral = choice("0",
                              joined(sequence(range("1", "9"),
                                              joined(repeat0(DecimalDigit)))))
var SignedInteger = sequence(choice("+", "-", ""), DecimalIntegerLiteral)
var ExponentIndicator = choice("e", "E")
var ExponentPart = sequence(ExponentIndicator, SignedInteger)
var DecimalLiteral = choice(action(joined(sequence(DecimalIntegerLiteral, ".",
                                                   repeat0(DecimalDigit),
                                                   action(optional(ExponentPart),
                                                     function(ast) { return ast || "" }))),
                                   function(ast) { return parseFloat(ast) }),
                            action(joined(sequence(DecimalIntegerLiteral,
                                                   action(optional(ExponentPart),
                                                     function(ast) { return ast || ""}))),
                                   function(ast) { return parseInt(ast) }))

var HexDigit = choice(range("0", "9"), range("a", "f"), range("A", "F"))
var HexIntegerLiteral = action(joined(sequence(choice("0x", "0X"), joined(repeat1(HexDigit)))),
  function(ast) { return parseInt(ast) })
var NumericLiteral = choice(HexIntegerLiteral, DecimalLiteral)

var SingleEscapeCharacter = choice("'", "\"", "\\", "b", "f", "n", "r", "t", "v"
)
var NonEscapeCharacter = negate(SingleEscapeCharacter)

var CharacterEscapeSequence = choice(SingleEscapeCharacter, NonEscapeCharacter)
var HexEscapeSequence = joined(sequence("x", HexDigit, HexDigit))
var UnicodeEscapeSequence = joined(sequence("u", HexDigit, HexDigit, HexDigit, HexDigit))
var EscapeSequence = choice(HexEscapeSequence, UnicodeEscapeSequence, CharacterEscapeSequence)
var DoubleStringCharacter = choice(negate(choice("\"", "\\", "\r", "\n")),
                                   joined(sequence("\\", EscapeSequence)))
var StringLiteral = action(joined(sequence("\"", joined(repeat0(DoubleStringCharacter)), "\"")),
  function(ast) { return eval(ast) })   // XXX ick

/* end of adapted code */

var Literal = action(choice(NumericLiteral, StringLiteral),
  function(ast) { return { type: "lit", expr: ast } })

var MatchParen = action(wsequence(expect("("), MatchTerm, expect(")")),
  function(ast) { return ast[0] })
var BoundMatch = action(wsequence(Identifier, "@", MatchTerm),
  function(ast) { return { type: "bound", name: ast[0], expr: ast[2] } })
var MatchTerm = whitespace(choice(BoundMatch,
                                  Var,
                                  Constructor,
                                  Literal,
                                  MatchParen))
var MatchTerms = choice(action(wsequence(repeat0(MatchTerm), "if", Expr),
                               function(ast) { return [{ type: "guard", guard: ast[2], term: ast[0] }] }),
                       repeat0(MatchTerm))

var fun_bind = action(wsequence(Identifier, MatchTerms, "=", Expr),
                      function(ast) { return [[ast[0], { type: "fn",
                                                         fns: [{args: ast[1], expr: ast[3]}] }]] })

var single_bind = choice(action(wsequence(Identifier, "=", Expr),
                                function(ast) { return [[ast[0], ast[2]]] }),
                         fun_bind)

var multi_bind = repeat1(action(wsequence("|", single_bind),
  function(ast) { return ast[1][0] }))

var Tuple = action(wsequence("(", lossyList(Expr, ","), ")"),
  function(ast) { if (ast[1].length == 1)
                    return ast[1][0] // no 1-tuples
                  else
                    return { type: "tuple", elts: ast[1] } })

var RecordKey = choice(Identifier, Constructor, Literal)

var RecordElt = action(wsequence(RecordKey, ":", Expr),
  function(ast) { return [ast[0], ast[2]] })

var Record = action(wsequence("{", lossyList(RecordElt, ","), "}"),
  function(ast) { return { type: "record", elts: ast[1] } })

var FunDef1 = action(wsequence(MatchTerms, token("->"), Expr),
                      function(ast) { return [{args: ast[0], expr: ast[2]}] })
var FunDef = choice(repeat1(action(wsequence("|", MatchTerms, token("->"), Expr),
                      function(ast) { return {args: ast[1], expr: ast[3]} })),
                    FunDef1)

var Fn = action(wsequence(token("fn"), FunDef),
  function(ast) { return { type: "fn", fns: ast[1] } })

var Let = action(wsequence(token("let"), choice(single_bind, multi_bind), token("in"), Expr),
  function(ast) { return { type: "let", bindings: ast[1], expr: ast[3] } })

var DefLet = action(wsequence(token("let"), choice(single_bind, multi_bind)),
  function(ast) { return { type: "def", bindings: ast[1] } })

var Paren = action(sequence(expect("("), Expr, expect(")")),
  function(ast) { return ast[0] })

var ExprNoApp = choice(Literal, Constructor, Tuple, Record, Fn, Let, Var, Paren)

var App = action(wsequence(ExprNoApp, ExprNoOp),
  function(ast) { return { type: "app", expr: ast[0], arg: ast[1] } })

var ExprNoOp = choice(Literal, Constructor, Tuple, Record, Fn, App, Let, Var, Paren)

function infixl_op(name) {
  return action(whitespace(name),
                function(ast) { return function(lhs, rhs) {
                  return { type: "app",
                           expr: { type: "app", expr: {type:"var",name:name}, arg: lhs },
                           arg: rhs }
                }})
}

function infixr_op(name) {
  return action(whitespace(name),
                function(ast) { return function(lhs, rhs) {
                  return { type: "app",
                           expr: { type: "app", expr: {type:"var",name:name}, arg: rhs },
                           arg: lhs }
                }})
}

infix_op = infixl_op

function nonfix(p, s) {
  var p = toParser(p)
  return action(sequence(p, optional(sequence(s, p))),
                function(ast) {
                  return ast[1] ? ast[1][0](ast[0], ast[1][1]) : ast[0];
                });
}

// strongest
// left-associative
var Infix7 = chainl(whitespace(ExprNoOp), choice(infixl_op('*'), infixl_op('/')))
var Infix6 = chainl(whitespace(Infix7), choice(infixl_op('+'), infixl_op('-')))
// non-associative
var Infix4 = nonfix(whitespace(Infix6), choice(infix_op('==')))
// right-associative
var Infix3 = chainl(whitespace(Infix4), choice(infixr_op('||'), infixr_op('&&')))
var Infix0 = chainl(whitespace(Infix3), choice(infixr_op('$')))
// weakest

// TODO: build list of infix levels from table.
//       avoid + trying to match ++

var Infix = Infix0

var Expr = choice(Literal, Constructor, Tuple, Record, Fn, Let, DefLet, Infix, App, Var, Paren)

print(Expr(ps("let a = b in c")).toSource())
print(Expr(ps("let | a = b | c = d in c")).toSource())
print(Expr(ps("let a = b in let c = d in c")).toSource())
print(Expr(ps("a (b bb) c")).toSource())

print(Expr(ps("a + (b bb) + c")).toSource())
print(Expr(ps("a + b bb + c")).toSource())

print(Expr(ps("a || b || c || d")).toSource())

function sexpr(ast) {
  switch(ast.type) {
  case "app": return "(" + sexpr(ast.expr) + " " + sexpr(ast.arg) + ")"
  case "var": return ast.name
  default: return "<undefined " + ast.toSource() + ">"
  }
}

print(sexpr(Expr(ps("a + b + c")).ast))
print(sexpr(Expr(ps("a || b || c+cc+ccc || d")).ast))

print(sexpr(Expr(ps("a && b || bb && c")).ast))

print(Expr(ps("a == b == c")).toSource())
print(sexpr(Expr(ps("a == b == c")).ast))

print(sexpr(Expr(ps("a $ b c == d $ c")).ast))
print(sexpr(Expr(ps("a $ b (c == d) $ c")).ast))

print(sexpr(Expr(ps("a * b + c == a * b + a * c")).ast))

print(Expr(ps("identifier")).toSource())
print(Expr(ps("ident1f1er")).toSource())
print(Expr(ps("identifier'")).toSource())
print(Expr(ps("identifier''")).toSource())
print(Expr(ps("_identifier")).toSource())
print(Expr(ps("$identifier")).toSource())
print(Expr(ps("id-en-ti-fi-er")).toSource())
print(Expr(ps("(+) a b")).toSource())
print(Expr(ps("a + b")).toSource())

print(Expr(ps("a-b")).toSource())
print(Expr(ps("a - b")).toSource())

print(Expr(ps("2")).toSource())
print(Expr(ps("2.8")).toSource())
print(Expr(ps("0xff")).toSource())

print(Expr(ps('"0x\\nff"')).toSource())

print(Expr(ps("fn x -> y")).toSource())
print(Expr(ps("fn -> y")).toSource())

print(Expr(ps("fn (A b x) (B c d) -> y")).toSource())

print(Expr(ps("fn 0 1 -> y")).toSource())

print(Expr(ps("fn Foo (1 , 2) b -> z")).toSource())

print(Expr(ps("fn z=(Foo a b) b -> z")).toSource())

print(Expr(ps("fn a if b -> z")).toSource())

print(Expr(ps("{a: b}")).toSource())

print(Expr(ps("{}")).toSource())

print(Expr(ps("{a: b, c: d,}")).toSource())

print(Expr(ps("fn | 0 -> 1 | 2 -> 3")).toSource())

print(Expr(ps("let a = b in c")).toSource())

print(Expr(ps("let a x = b")).toSource())

