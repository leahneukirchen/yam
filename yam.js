load("jsparse/jsparse.js")
//memoize = false                 // for debugging, less cluttered output

function joined(p) { return join_action(p, "") }

function optOr(p, def) {
  return action(optional(p),
    function(ast) { return ast || def })
}

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
var MatchTerm = function(state) { return MatchTerm(state); }
var TopLevelExpr = function(state) { return TopLevelExpr(state); }
var Identifier = function(state) { return Identifier(state); }

var ReservedWord = choice("end", "fn", "if",
                          "infixl", "infixr", "infix", "in",
                          "let", "module")
var ReservedOperator = choice("|", "@", "->", "=")

var IdentifierBeginning = choice(range("a", "z"), "_")
var IdentifierPart = choice(range("a", "z"), range("A", "Z"),
                            range("0", "9"), "_",
                            sequence("-", and(range("a", "z"), range("A", "Z"),
                                              range("0", "9"))),
                            "'")
var IdentifierEnd = choice(range("a", "z"), range("A", "Z"),
                            range("0", "9"), "_", "'", "!", "?")

var IdentifierName = joined(
  sequence(IdentifierBeginning,
           joined(repeat0(IdentifierPart)),
           joined(repeat0(IdentifierEnd))))

var ConstructorBeginning = choice(range("A", "Z"))
var ConstructorPart = choice(range("a", "z"), range("A", "Z"),
                             range("0", "9"))
var ConstructorEnd = ConstructorPart

var ConstructorName = joined(
  sequence(ConstructorBeginning,
           joined(repeat0(ConstructorPart)),
           joined(repeat0(ConstructorEnd))))

var Constructor = action(sequence(ConstructorName, Expr),
  function(ast) { return { type: "cons", name: ast[0], expr: ast[1] } })

var Operator = choice(butnot(joined(repeat1(
  choice("!", "#", "$", "%", "&", "*", "+", "-", ".", "/", ":",
         "<", "=", ">", "?", "@", "\\", "^", "|", "~"))), ReservedOperator),
  action(sequence("`", Identifier, "`"),
    function(ast) { return ast[1] }))

var OperatorName = action(sequence(expect("("), Operator, expect(")")),
  function(ast) {
    if (!opDefined(ast[0])) {
      defineOp(ast[0], "left", 9)
      updateOpParser()
    }
    return ast[0]
  })

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
var SignedInteger = joined(sequence(optOr(choice("+", "-"), ""), DecimalIntegerLiteral))
var ExponentIndicator = choice("e", "E")
var ExponentPart = joined(sequence(ExponentIndicator, SignedInteger))
var DecimalLiteral = choice(action(joined(sequence(SignedInteger, ".",
                                                   repeat0(DecimalDigit),
                                                   optOr(ExponentPart, ""))),
                                   function(ast) { return parseFloat(ast) }),
                            action(joined(sequence(SignedInteger,
                                                   optOr(ExponentPart, ""))),
                                   function(ast) { return parseFloat(ast) }))

var HexDigit = choice(range("0", "9"), range("a", "f"), range("A", "F"))
var HexIntegerLiteral = action(joined(sequence(choice("0x", "0X"), joined(repeat1(HexDigit)))),
  function(ast) { return parseInt(ast) })

var OctalDigit = choice(range("0", "7"))
var OctalIntegerLiteral = action(joined(sequence("0", joined(repeat1(OctalDigit)))),
  function(ast) { return parseInt(ast) })

var NumericLiteral = choice(HexIntegerLiteral, OctalIntegerLiteral, DecimalLiteral)

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

/* end of adapted code.  */

var Literal = action(choice(NumericLiteral, StringLiteral),
  function(ast) { return { type: "lit", expr: ast } })

var Tuple = action(wsequence("(", lossyList(Expr, ","), ")"),
  function(ast) { if (ast[1].length == 1)
                    return ast[1][0] // no 1-tuples
                  else
                    return { type: "tuple", elts: ast[1] } })

var MatchParen = action(wsequence(expect("("), MatchTerm, expect(")")),
  function(ast) { return ast[0] })
var BoundMatch = action(wsequence(Identifier, "@", MatchTerm),
  function(ast) { return { type: "bound", name: ast[0], expr: ast[2] } })
var MatchTerm = whitespace(choice(BoundMatch,
                                  Var,
                                  Constructor,
                                  Literal,
                                  Tuple,
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

var Let = action(wsequence(token("let"), choice(single_bind, multi_bind), butnot(token("in"), "infix"), Expr),
  function(ast) { return { type: "let", bindings: ast[1], expr: ast[3] } })

var DefLet = action(wsequence(token("let"), choice(single_bind, multi_bind)),
  function(ast) { return { type: "def", bindings: ast[1] } })

var Paren = action(sequence(expect("("), Expr, expect(")")),
  function(ast) { return ast[0] })

var Raw = action(sequence("{%",
                          joined(repeat1(choice(negate('%'),
                                                joined(sequence('%', not('}')))))),
                          "%}"),
  function(ast) { return { type: "raw", code: ast[1] } })

var ExprNoApp = choice(Literal, Constructor, Tuple, Record, Fn, Let, Var, Paren, Raw)

var App = action(wsequence(ExprNoApp, repeat1(whitespace(ExprNoApp))),
  function(ast) { return foldl(function(lhs, rhs) {
    return { type: "app", expr: lhs, arg: rhs }
  }, ast[0], ast[1]) })

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

var Infix10 = chainl(whitespace(ExprNoOp),
                     action(whitespace(sequence("`", Identifier, "`")),
  function(ast) { return function(lhs, rhs) {
    return { type: "app",
             expr: { type: "app", expr: {type:"var", name: ast[1] }, arg: lhs },
             arg: rhs } }}))

function opDefined(op) {
  for (var i = 0; i < ops.length; i++)
    if (ops[i].name == op)
      return true
  return false
}

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

  parsers[tbl.length] = Infix10
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

var CompilationUnit = repeat0(action(sequence(whitespace(TopLevelExpr),
                                              optional(whitespace(""))),
                                     function(ast){return ast[0]}))
