when false:
  proc Command*(name: string | NimNode, arguments: varargs[NimNode]): NimNode =
    result = nnkCommand.newTree(name.asIdent)
    result.addArgs arguments

  template name*(x: NimNode): untyped =
    case x.kind:
    of nnkCommand, nnkCall, nnkInfix, nnkPrefix, nnkPostfix, nnkCallStrLit, nnkBlockStmt, nnkIdentDefs:
      x[0]
    of nnkIdent:
      x.strVal
    else:
      raise newException(ValueError, "Unable to get name for NimNode of kind " & $x.kind)

  template `name=`*(x, val: NimNode): untyped =
    case x.kind:
    of nnkCommand, nnkCall, nnkInfix, nnkPrefix, nnkPostfix, nnkCallStrLit, nnkBlockStmt, nnkIdentDefs:
      x[0] = val
    else:
      raise newException(ValueError, "Unable to set name for NimNode of kind " & $x.kind)
import macros

macro generate(nodes: untyped): untyped =
  echo nodes.treeRepr
  assert(nodes.kind == nnkStmtList)
  for node in nodes:
    assert(node.kind == nnkCall)
    assert(node[0].kind == nnkIdent)
    assert(node[1].kind == nnkStmtList)
    var
      arguments: set[uint16]
      allAfter = -1
      allUntil = -1
    for arg in node[1]:
      assert(arg.kind == nnkCall)
      assert(arg[0].kind == nnkBracketExpr)
      assert(arg[0][0].kind == nnkIdent)
      case arg[0][1].kind:
      of nnkIntLit:
        arguments.incl arg[0][1].intVal.uint16
      of nnkInfix:
        assert(arg[0][1][0].kind == nnkIdent)
        case $arg[0][1][0]:
        of "..":
          for i in arg[0][1][1].intVal..arg[0][1][2].intVal:
            arguments.incl i.uint16
        of "..<":
          for i in arg[0][1][1].intVal..<arg[0][1][2].intVal:
            arguments.incl i.uint16
        of "..^":
          allAfter = arg[0][1][1].intVal.int
          allUntil = arg[0][1][2].intVal.int
      else:
        assert(arg[0][1].kind in {nnkIntLit, nnkInfix})
    echo arguments
    echo allAfter
    echo allUntil

generate:
  Command:
    name[0](string | NimNode):
      field = toIdent(name)
    arguments[1..^1](varargs[NimNode]):
      when not isInitialiser:
        field.del(1, field.len - 1)
      field.add(children = arguments)

when false:
  proc Command*(name: string | NimNode, arguments: varargs[NimNode]): NimNode =
    const isInitialiser = true
    result = nnkCommand.newTree(toIdent(name))
    when not isInitialiser:
      field.del(1, field.len - 1)
    result.add(children = arguments)

macro test(x: untyped): untyped =
  echo x.treeRepr

test(0..^1)

var x = [1, 2 ,3]

echo x[1 ..^ 1]
echo x[1 .. ^1]
