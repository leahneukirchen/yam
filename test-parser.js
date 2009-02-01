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

ok(parses("a $ b && c"))
ok(parses("a && b $ c"))
ok(parses("a $ b c + d"))
ok(parses("a + b c $ d"))
ok(parses("a $ b c == d"))
ok(parses("a == b c $ d"))
ok(parses("a + b $ c"))
same_ast("a $ b c == d $ c", "a $ ((b c) == d) $ c")

same_ast("a + b", "((+) a) b")

ok(!parses("a == b == c"))

ok(identifier("identifier"))
ok(identifier("ident1f1er"))
ok(identifier("_identifier"))
ok(identifier("identifier'"))
ok(identifier("identifier''"))
ok(identifier("ident'fier"))
ok(identifier("identifier?"))
ok(identifier("id-en-ti-fi-er"))
ok(identifier("iDeNtIfIeR"))
ok(identifier("a"))

ok(!identifier("let"))
ok(!identifier("infixr"))
ok(!identifier("infixl"))
ok(!identifier("infix"))
ok(!identifier("in"))
ok(!identifier("0identifier"))
ok(!identifier("Identifier"))
diag("- at the end")
ok(!identifier("foo-"))

ok(identifier("(+)", "+"))
ok(identifier("(==)", "=="))
ok(identifier("(!=)", "!="))

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

ok(literal("0xff", 0xff))
diag("octal")
ok(literal("077", 077))

ok(literal('"foo"', "foo"))
ok(literal('"fo\\no"', "fo\no"))

/* Unsupported JavaScript syntaxes.  */
ok(!literal(".2", .2))
ok(!literal("'foo'", "foo"))

/* XXX To be extended.  */
ok(parses("fn x -> y"))
ok(parses("fn -> y"))
ok(parses("fn (A b x) (B c d) -> y"))
ok(parses("fn 0 1 -> y"))
ok(parses("fn Foo (1 , 2) b -> z"))
ok(parses("fn z@(Foo a b) b -> z"))
ok(parses("fn a if b -> z"))
ok(parses("{a: b}"))
ok(parses("{}"))
ok(parses("{a: b, c: d,}"))
ok(parses("fn | 0 -> 1 | 2 -> 3"))
ok(parses("let a = b in c"))
ok(parses("let a x = b"))
ok(parses("(fn x y -> y) 6 7"))


ok(parses("let a = b let c = d"))

ok(parses("module z let a = b end"))

ok(parses("a; b a; c"))

ok(parses("a.b"))

ok(parses("a.b.c"))

ok(parses("(a + b).c"))

ok(parses("a.0.1"))

ok(parses("a.1.A"))

ok(parses("{a: b, A: b, 0: b}"))

ok(parses("{% foo %}"))

ok(parses("{% foo % bar %}"))

same_ast("infixl * 6 infixl / 6 infixl + 7 + infixl - 7 a + b * c",
         "infixl * 7 infixl / 7 infixl + 6 + infixl - 6 (a + b) * c")

same_ast("infixr <=> 7 a <=> b <=> c", "infixr <=> 7 a <=> (b <=> c)")
same_ast("infixl <=> 7 a <=> b <=> c", "infixl <=> 7 (a <=> b) <=> c")
ok(!parses("infix <=> 9 a <=> b <=> c"))

ok(parses("infixl ++ 3 c ++ d + e"))


p_ast("infix == 4\n\
foo\n\
infix < 4")