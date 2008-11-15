load('comp.js')

while(true) {
  print("; ")
  var line = readline()
  
  try {
    print(code = compile(line))
    eval(code)
  } catch(e) {
    print(e)
    print(e.stack)
  }
}