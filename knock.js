function ok(test, msg) {
  if (test)
    print("ok")
  else {
    if (msg) 
      print("not ok - " + msg)
    else
      print("not ok")
  }
}

function diag(msg) {
  print(msg.replace(/^/, "# "))
}

function p(o) {
  if (typeof(o) == 'undefined')
    diag("undefined")
  else
    diag(o.toSource())
}