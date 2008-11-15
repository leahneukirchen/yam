load('comp.js')

$silent = typeof($silent) == "undefined" ? false : $silent

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
  var f
  try {
    f = new File(file)
    f.open("read")
  } catch(e) {
    f = new File(file + ".ym")
    f.open("read")
  }
  global_eval (code = compile (f.readAll().join("\n")))
  if (!$silent)
    print(code)
}

require("Prelude.ym")

function repl() {
  while(true) {
    prompt("; ")
    var line = gets()
    
    if (line == null)
      break
    
    if (line.match(/^[ \n]*$/))
      continue
    
    try {
      var code = compile(line)
      if (!$silent) {
        print(";; code:")
        print(code)
      }
      var result = eval(code)
      if (!$silent) {
        print(";; result:")
      }
      if (!$silent || result != undefined)
        print(result)
    } catch(e) {
      print(e)
      print(e.stack)
    }
  }
}
