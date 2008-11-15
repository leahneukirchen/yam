load("jsparse/jsparse.js")
memoize = false                 // for debugging, less cluttered output

function joined(p) { return join_action(p, "") }

/* Allow optional seperator at the end.  */
function lossyList(p, s) {
  return action(optional(sequence(wlist(p, s), optional(s))),
    function(ast) { return ast[0] ? ast[0] : [] })
}
function lossyList2(p, s) {
  return action(sequence(p, s, wlist(p, s), optional(s)),
    function(ast) { return [ast[0]].concat(ast[2]) })
}

/* forward declarations.  */
var Expr = function(state) { return Expr(state); }
var ExprNoOp = function(state) { return ExprNoOp(state); }
var MatchTerm = function(state) { return MatchTerm(state); }
var TopLevelExpr = function(state) { return TopLevelExpr(state); }

var ReservedWord = choice("end", "fn", "if", "in",
                          "infix", "infixl", "infixr",
                          "let", "module")
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

var RecordKey = choice(Identifier, ConstructorName,
                       DecimalIntegerLiteral)

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

var Sel = action(wsequence(ExprNoApp, repeat1(wsequence(".", RecordKey))),
  function(ast) { return foldl(function(lhs, rhs) {
    return { type: "sel", expr: lhs, field: rhs[1] }
  }, ast[0], ast[1]) })

var ExprNoOp = choice(Sel, App, ExprNoApp)

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
    // XXX ugly, but works
    function(ast) { return function(lhs, rhs) {
      var head = lhs
      while (head.type == "app" && head.arg.type == "app") {
        head = head.arg
      }
      if (head.type == "app") {
        head.arg = { type: "app", expr: { type: "app", expr: { type: "var", name: name }, arg: head.arg }, arg: rhs }
        return lhs
      } else {
        return { type: "app", expr: { type: "app", expr: { type: "var", name: name }, arg: lhs }, arg: rhs }
      }
    } } )
}

infix_op = infixl_op

function nonfix(p, s) {
  var p = toParser(p)
  return action(sequence(p, optional(sequence(s, p))),
                function(ast) {
                  return ast[1] ? ast[1][0](ast[0], ast[1][1]) : ast[0];
                });
}

var ops = []

function defineOp(op, assoc, precedence) {
  for (var i = 0; i < ops.length; i++)
    if (ops[i].name == op)
      ops.splice(i, 1)          // delete entry
  ops.push({name: op, assoc: assoc, prec: precedence})
}

function findChain(ops) {
  var chain
  for (var i = 0; i < ops.length; i++) {
    if (ops[i].assoc == "none") {
      if (chain == chainl)
        throw("Mixed assoc levels in " + ops.toSource())
      chain = nonfix
    } else {
      if (chain == nonfix)
        throw("Mixed assoc levels in " + ops.toSource())
      chain = chainl
    }
  }
  return chain || chainl
}

function buildChoice(ops) {
  var parsers = []
  for (var i = 0; i < ops.length; i++) {
    switch(ops[i].assoc) {
    case "left":
      parsers.push(infixl_op(ops[i].name))
      break
    case "right":
      parsers.push(infixr_op(ops[i].name))
      break
    case "none":
      parsers.push(infix_op(ops[i].name))
      break
    default:
      throw("Unknown assoc level: " + ops[i].assoc)
    }
  }
  return choice.apply(choice, parsers)
}

function buildOpParser(ops) {
  ops = ops.sort(function(a, b) { return a.prec - b.prec })
  var tbl = []
  for (var i = 0; i < ops.length; i++) {
    var prec = ops[i].prec
    tbl[prec] = tbl[prec] || []
    tbl[prec].push(ops[i])
  }

  var parsers = []

  parsers[tbl.length] = ExprNoOp
  for (var i = tbl.length - 1; i >= 0; i--) {
    tbl[i] = tbl[i] || []
    parsers[i] = findChain(tbl[i])(whitespace(parsers[i+1]), buildChoice(tbl[i]))
  }
  return parsers[0]
}

function updateOpParser() {
  opParser = buildOpParser(ops)
}

defineOp('*', 'left', 7)
defineOp('/', 'left', 7)
defineOp('+', 'left', 6)
defineOp('-', 'left', 6)
defineOp('==', 'none', 4)
defineOp('||', 'right', 3)
defineOp('&&', 'right', 3)
defineOp('$', 'right', 0)

updateOpParser()

/* dynamic hook to allow updating the table.  */
var Infix0 = function(state) { return opParser(state) }

var Infix = chainl(whitespace(Infix0), action(whitespace(';'),
  function(ast) { return function(lhs, rhs) {
                    if (lhs.type == "seq")
                      return { type: "seq", exprs: lhs.exprs.concat(rhs) }
                    else
                      return { type: "seq", exprs: [lhs, rhs] }}}))

var Module = action(wsequence("module", repeat0(TopLevelExpr), "end"),
  function(ast) { return { type: "module", exprs: ast[1] } })

var NamedModule = action(wsequence("module", Identifier,
                                   repeat0(TopLevelExpr), "end"),
  function(ast) { return { type: "def", bindings:
                           [[ast[1], { type: "module", exprs: ast[2] }]]
                         }})

var Expr = choice(App, Infix, ExprNoOp, Module)

var OpDecl = action(choice(action(wsequence("infixl", Operator, DecimalLiteral),
                                  function(ast) {
                                    defineOp(ast[1], 'left', ast[2])
                                    updateOpParser()
                                  }),
                           action(wsequence("infixr", Operator, DecimalLiteral),
                                  function(ast) {
                                    defineOp(ast[1], 'right', ast[2])
                                    updateOpParser()
                                  }),
                           action(wsequence("infix", Operator, DecimalLiteral),
                                  function(ast) {
                                    defineOp(ast[1], 'none', ast[2])
                                    updateOpParser()
                                  })),
                    function(ast) { return { type: "noop" } })

var TopLevelExpr = choice(OpDecl, NamedModule, Expr, DefLet)

var CompilationUnit = repeat0(TopLevelExpr)

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

print(Expr(ps("(fn x y -> y) 6 7")).toSource())

print(CompilationUnit(ps("let a = b let c = d")).toSource())

print(CompilationUnit(ps("module z let a = b end")).toSource())

print(CompilationUnit(ps("a; b a; c")).toSource())

print(sexpr(CompilationUnit(ps(  "infixl * 6 "
                               + "infixl / 6 "
                               + "infixl + 7 "
                               + "infixl - 7 "
                               + "a + b * c")).ast[4]))

print(sexpr(CompilationUnit(ps(  "infixl * 7 "
                               + "infixl / 7 "
                               + "infixl + 6 "
                               + "infixl - 6 "
                               + "a + b * c")).ast[4]))

print(sexpr(CompilationUnit(ps(  "infixr <=> 7 "
                               + "a <=> b <=> c")).ast[1]))
print(sexpr(CompilationUnit(ps(  "infixl <=> 7 "
                               + "a <=> b <=> c")).ast[1]))
print(sexpr(CompilationUnit(ps(  "infix <=> 9 "
                               + "a <=> b <=> c")).ast[1]))

print(Expr(ps("a.b")).toSource())

print(Expr(ps("a.b.c")).toSource())

print(Expr(ps("(a + b).c")).toSource())

print(Expr(ps("a.0.1")).toSource())

print(Expr(ps("a.1.A")).toSource())

print(Expr(ps("{a: b, A: b, 0: b}")).toSource())

print(CompilationUnit(ps(    "infixl ++ 3 "
                           + "c ++ d + e")).toSource())
