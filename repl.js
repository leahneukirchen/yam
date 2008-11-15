load('comp.js')

if (typeof(File)) {
  function prompt(p) {
    File.output.write(p)
    File.output.flush
  }
  function gets() {
    return File.input.readln()
  }
} else {
  function prompt(p) {
    print(p)
  }
  function gets() {
    return readline()
  }
}


function global_eval(code) {
  eval.call(null, code)
}

require = function(file) {
  var f = new File(file)
  f.open("read")
  global_eval (code = compile (f.readAll().join("\n")))
  print(code)
}

require("Prelude.ym")
print("")

while(true) {
  prompt("; ")
  var line = gets()

  if (line == null)
    break

  if (line.match(/^[ \n]*$/))
    continue

  try {
    var code = compile(line)
    print(";; code:")
    print(code)
    var result = eval(code)
    print(";; result:")
    print(result)
  } catch(e) {
    print(e)
    print(e.stack)
  }
}