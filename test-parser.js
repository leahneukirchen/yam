load('knock.js')

load('yam.js')

function p_ast(s) {
  var a = CompilationUnit(ps(s)).ast
  diag(a.toSource())
}

function parses(s) {
  var c = CompilationUnit(ps(s)).remaining
  return c.length == 0
}

function same_ast(s1, s2) {
  var a1 = CompilationUnit(ps(s1)).ast
  var a2 = CompilationUnit(ps(s2)).ast

  ok(a1.toSource() == a2.toSource(), "'" + s1 + "' and '" + s2 + "' differ")
  if(a1.toSource() != a2.toSource()) {
    p_ast(s1)
    p_ast(s2)
  }
}

function identifier(id, id2) {
  var a = Identifier(ps(id)).ast
  return a == (id2 || id)
}

function literal(lit, ex) {
  var a = Literal(ps(lit)).ast
  p(a)
  return a && a.type == "lit" && a.expr === ex
}

ok(parses("let a = b in c"))
ok(parses("let | a = b | c = d in c"))
ok(parses("let a = b in let c = d in c"))
ok(parses("a (b bb) c"))

ok(parses("a + (b bb) + c"))
ok(parses("a + b bb + c"))

ok(parses("a || b || c || d"))

same_ast("a b c", "a (b c)")
same_ast("a + b + c", "(a + b) + c")
same_ast("a + b + c + d", "((a + b) + c) + d")
same_ast("a || b || c", "a || (b || c)")
same_ast("a || b || c || d", "a || (b || (c || d))")

same_ast("a || b || c+cc+ccc || d", "a || b || (c+cc+ccc) || d")

same_ast("a && b || bb && c", "(a && b) || (bb && c)")

diag("$")
ok(parses("a $ b c + d"))
ok(parses("a $ b c == d"))
ok(parses("a + b $ c"))
// same_ast("a $ b c == d $ c", "a $ ((b c) == d) $ c")

same_ast("a + b", "((+) a) b")

ok(!parses("a == b == c"))

ok(identifier("identifier"))
ok(identifier("ident1f1er"))
ok(identifier("_identifier"))
ok(identifier("identifier'"))
ok(identifier("identifier''"))
ok(identifier("ident'fier"))
ok(identifier("identifier?"))
ok(identifier("$identifier"))
ok(identifier("id-en-ti-fi-er"))
ok(identifier("iDeNtIfIeR"))
ok(identifier("a"))

ok(!identifier("let"))
ok(!identifier("0identifier"))
ok(!identifier("Identifier"))

ok(identifier("(+)", "+"))
ok(identifier("(==)", "=="))
ok(identifier("(+~|<)", "+~|<"))
ok(!identifier("(->)", "->"))
ok(!identifier("(=)", "="))

ok(literal("1", 1))
diag("signed ints")
ok(literal("-1", -1))
ok(literal("1.2", 1.2))
ok(literal("0.2", 0.2))
diag("sci notation")
ok(literal("0.2E3", 0.2E3))

diag("octal")
ok(literal("0xff", 0xff))
ok(literal("077", 077))

ok(literal('"foo"', "foo"))
ok(literal('"fo\\no"', "fo\no"))

/* Unsupported JavaScript syntaxes.  */
ok(!literal(".2", .2))
ok(!literal("'foo'", "foo"))
