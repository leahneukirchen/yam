load('knock.js')

load('comp.js')

function p_ast(s) {
  var a = CompilationUnit(ps(s)).ast
  diag(a.toSource())
}

function compiles(s) {
  try {
    compile(s)
    return true
  } catch(e) {
    ok(0, e)
  }
}


ok(compiles("a b c"))

ok(compiles("a + b + c"))

ok(compiles("let a = b in c a"))

ok(compiles("let | a = b | b = a in c a"))

ok(compiles('fn x -> x + x'))

ok(compiles('fn x y -> y + x'))

ok(compiles('let pair x y = (x,y) in pair 1 2'))

ok(compiles('Cons (a,b)'))

ok(compiles('fn | 0 -> 1 | 1 -> 0'))

ok(compiles('fn | 0 0 -> 1\
                  | 0 1 -> 0\
                  | 1 0 -> 0\
                  | 1 1 -> 1\
'))

ok(compiles('fn | Pair(x,y) -> x | _ -> _'))
ok(compiles('fn | Pair(Pair(x,z),y) -> x | _ -> _'))

ok(compiles("fn | x y if x == y -> 1 | _ _ -> 0"))

ok(compiles("module z let a = b end"))

ok(compiles("module let a = b end"))

ok(compiles("a; b a; c"))

ok(compiles( "infixl * 6 "
             + "infixl / 6 "
             + "infixl + 7 "
             + "infixl - 7 "
             + "a + b * c"))
ok(compiles( "infixl * 7 "
             + "infixl / 7 "
             + "infixl + 6 "
             + "infixl - 6 "
             + "a + b * c"))
ok(compiles("a.1.A"))

ok(compiles("{% print(foo) %} "))

ok(compiles("{a: 1, b: 2+2}"))
ok(compiles("{A: 1, 4: 2+2}"))
