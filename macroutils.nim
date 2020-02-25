import macros

type
  Slice[offset, length: static[int]] = distinct NimNode

converter toLit(x: string): NimNode = newLit(x)

template len[offset, length: static[int]](s: Slice[offset, length]): untyped = length

template addArgs(x: NimNode, arguments: varargs[NimNode]): untyped =
  for a in arguments:
    x.add a

template asIdent(name: static[string] | NimNode): untyped =
  when name is NimNode:
    assert name.kind == nnkIdent, "Node must be in identifier"
    name
  else:
    newIdentNode(name)

template Command(name: static[string] | NimNode, arguments: varargs[NimNode]): untyped =
  var x = nnkCommand.newTree(name.asIdent)
  x.addArgs arguments
  x

template Call(name: static[string] | NimNode, arguments: varargs[NimNode]): untyped =
  var x = nnkCall.newTree(name.asIdent)
  x.addArgs arguments
  x

template Infix(name: static[string] | NimNode; arg1, arg2: untyped): untyped =
  nnkInfix.newTree(
    name.asIdent,
    arg1, arg2)

template name(x: NimNode): untyped =
  case x.kind:
  of nnkCommand, nnkCall, nnkInfix:
    x[0]
  else:
    raise newException(ValueError, "Unable to get name for NimNode of kind " & $x.kind)

template args(x: NimNode): untyped =
  case x.kind:
  of nnkCommand, nnkCall, nnkInfix:
    Slice[1, x.len - 1](x)
  else:
    raise newException(ValueError, "Unable to get arguments for NimNode of kind " & $x.kind)

template `[]`[offset, length: static[int]](s: Slice[offset, length], idx: int): untyped =
  s.NimNode[offset + idx]

template `[]=`[offset, length: static[int]](s: Slice[offset, length], idx: int, val: untyped): untyped =
  s.NimNode[offset + idx] = val

when isMainModule:
  macro testInfix(): untyped =
    result = Infix("&", "Hello " , "world")
    echo result.name
    echo result.args.len
    echo result.args[0]
    echo result.args[1]

  echo testInfix()
  #macro testCommand(): untyped =
  #  result = Command("echo", newLit("Hello world"))
  #  echo result.treeRepr
  #  result.args[0] = newLit("Hello there")
  #  echo result.args.len
  #  echo result.args[0]
  #  echo result.treeRepr

  #macro testCall(): untyped =
  #  result = Call("echo", newLit("Hello world"))
  #  echo result.treeRepr
  #  result.args[0] = newLit("Hello there")
  #  echo result.args.len
  #  echo result.args[0]
  #  echo result.treeRepr

  #testCommand()
  #testCall()
