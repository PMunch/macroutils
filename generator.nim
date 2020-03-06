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
import macros, tables#, algorithm

type
  Slice = object
    offset, length: int
    node: NimNode

template `[]`*(s: Slice, idx: int): untyped =
  assert idx < s.length, "Index out of bounds"
  s.node[s.offset + idx]

template `[]=`*(s: Slice, idx: int, val: untyped): untyped =
  assert idx < s.length, "Index out of bounds"
  s.node[s.offset + idx] = val

iterator items*(s: Slice): NimNode =
  for i in 0..<s.length:
    yield s.node[s.offset + i]

template len*(s: Slice): untyped = s.length

proc `$`*(s: Slice): string =
  result = "["
  for i in 0..<s.length:
    result.add s.node[s.offset + i].repr
    if i != s.length - 1:
      result.add ", "
  result.add "]"

macro generate(nodes: untyped): untyped =
  result = newStmtList()
  echo nodes.treeRepr
  echo nodes.repr
  assert(nodes.kind == nnkStmtList)
  var fields: Table[string, seq[tuple[kind, node: NimNode]]]
  let isInitialiser = newIdentNode("isInitialiser")
  for node in nodes:
    assert(node.kind == nnkCall)
    assert(node[0].kind == nnkIdent)
    assert(node[1].kind == nnkStmtList)
    var
      positives: seq[BiggestInt]
      negatives: seq[BiggestInt]
      flexible = (start: -1.BiggestInt, stop: -1.BiggestInt, node: newEmptyNode())
    for arg in node[1]:
      assert(arg.kind == nnkCall)
      assert(arg[0].kind == nnkBracketExpr)
      assert(arg[0][0].kind == nnkIdent)
      case arg[0][1].kind:
      of nnkIntLit:
        positives.add arg[0][1].intVal
      of nnkInfix:
        assert(arg[0][1][0].kind == nnkIdent)
        case $arg[0][1][0]:
        of "..":
          for i in arg[0][1][1].intVal..arg[0][1][2].intVal:
            positives.add i
        of "..<":
          for i in arg[0][1][1].intVal..<arg[0][1][2].intVal:
            positives.add i
        of "..^":
          flexible = (start: arg[0][1][1].intVal, stop: arg[0][1][2].intVal, node: arg)
      of nnkPrefix:
        negatives.add arg[0][1][1].intVal
      else:
        assert(arg[0][1].kind in {nnkIntLit, nnkInfix})
    #positives.sort
    #negatives.sort
    for i, v in positives:
      assert(i == v)
    if flexible.start != -1:
      assert(positives.len == 0 or positives[^1]+1 == flexible.start)
      assert(negatives.len == 0 or negatives[^1]+1 == flexible.stop)
      assert(positives.len != 0 or flexible.start == 0)
      assert(negatives.len != 0 or flexible.stop == 1)
    for i, v in negatives:
      assert(i+1 == v)
    assert(negatives.len == 0 or flexible.start != -1)
    var generator = nnkProcDef.newTree(
      nnkPostfix.newTree(newIdentNode("*"), node[0]), newEmptyNode(), newEmptyNode(),
      nnkFormalParams.newTree(newIdentNode("NimNode")), newEmptyNode(), newEmptyNode(), newStmtList())
    for arg in node[1]:
      generator[3].add nnkIdentDefs.newTree(arg[0][0], arg[1], newEmptyNode())
    let
      nodeKind = newIdentNode("nnk" & $node[0])
      argcount =
        if flexible.start == -1:
          newLit(positives.len)
        else:
          nnkInfix.newTree(newIdentNode("+"),
            newLit(positives.len + negatives.len), nnkDotExpr.newTree(flexible.node[0][0], newIdentNode("len")))
    generator[6].add quote do:
      const `isInitialiser` = true
      result = newNimNode(`nodeKind`)
      #result.sons = newSeq[NimNode](`argcount`)
      for i in 0..<`argcount`:
        result.add newEmptyNode()
    for arg in node[1]:
      if fields.hasKeyOrPut(arg[0][0].strVal, @[(kind: node[0], node: arg)]):
        fields[arg[0][0].strVal].add (kind: node[0], node: arg)
      if arg.len == 3:
        generator[6].add arg[2]
      else:
        # Add in some default setters
        case arg[0][1].kind:
        of nnkIntLit, nnkPrefix:
          let
            pos = arg[0][1]
            argName = arg[0][0]
          generator[6].add quote do:
            result[`pos`] = `argName`
        of nnkInfix:
          case $arg[0][1][0]:
          of "..", "..<":
            let
              argName = arg[0][0]
              theRange = arg[0][1]
              start = arg[0][1][1]
            generator[6].add quote do:
              for i in `theRange`:
                result[i] = `argName`[i-`start`]
          of "..^":
            let
              argName = arg[0][0]
              start = arg[0][1][1]
            generator[6].add quote do:
              for i in 0..`argName`.high:
                result[`start` + i] = `argName`[i]
        else: discard
    echo generator.treeRepr
    result.add generator
  for field, nodes in fields:
    let
      nameNode = newIdentNode(field)
      setterNameNode = newIdentNode(field & "=")
      x = newIdentNode("x")
      val = newIdentNode(field)
    var getter = quote do:
      template `nameNode`*(`x`: NimNode): untyped =
        case `x`.kind:
        else:
          raise newException(ValueError, "Unable to get " & `field` & " for NimNode of kind " & $`x`.kind)
    var setter = quote do:
      template `setterNameNode`*(`x`: NimNode, `val`: untyped): untyped =
        const `isInitialiser` = false
        template result(): untyped = `x`
        case `x`.kind:
        else:
          raise newException(ValueError, "Unable to set " & `field` & " for NimNode of kind " & $`x`.kind)
    for node in nodes:
      let
        getterBranch = nnkOfBranch.newTree(newIdentNode("nnk" & node.kind.strVal))
        setterBranch = nnkOfBranch.newTree(newIdentNode("nnk" & node.kind.strVal))
        indices = node.node[0][1]
      case indices.kind:
      of nnkIntLit, nnkPrefix:
        getterBranch.add quote do:
          `x`[`indices`]
        if node.node.len == 3:
          setterBranch.add node.node[2]
        else:
          setterBranch.add quote do:
            `x`[`indices`] = `val`
      of nnkInfix:
        case $indices[0]:
        of "..", "..<", "..^":
          let
            start = indices[1].intVal
            stop = indices[2].intVal
            length = stop - start + (if $indices[0] == "..<": 0 else: 1)
          if $indices[0] == "..^":
            getterBranch.add quote do:
              Slice(offset: `start`, length: `x`.len - `start` - `stop` + 1, node: `x`)
            setterBranch.add quote do:
              `x`.del(`start`, `x`.len - `stop` - `start` + 1)
              for i, v in `val`:
                `x`.insert(i + `start`, v)
          else:
            getterBranch.add quote do:
              Slice(offset: `start`, length: `length`, node: `x`)
            setterBranch.add quote do:
              assert `val`.len == `length`, "Unable to set fixed size field to different length: " & `field` & " in node of kind " & $`x`.kind
              for i, v in `val`:
                `x`[i + `start`] = v
      else: discard
      getter[6][0].insert 1, getterBranch
      setter[6][2].insert 1, setterBranch
    result.add getter
    result.add setter
  echo result.repr


macro createLitConverters(list: varargs[untyped]): untyped =
  result = newStmtList()
  let x = newIdentNode("x")
  for kind in list:
    result.add quote do:
      converter Lit*(`x`: `kind`): NimNode = newLit(`x`)

createLitConverters(char, int, int8, int16, int32, int64, uint, uint8, uint16,
                    uint32, uint64, bool, string, float32, float64, enum, object, tuple)

converter Lit*[N, T](x: array[N, T]): NimNode = newLit(`x`)
converter Lit*[T](x: seq[T]): NimNode = newLit(`x`)
converter Lit*[T](x: set[T]): NimNode = newLit(`x`)

proc asIdent(name: string | NimNode): NimNode =
  when name is NimNode:
    assert name.kind == nnkIdent, "Node must be an identifier, but was: " & $name.kind & "(" & name.repr & ")"
    name
  else:
    newIdentNode(name)

#generate:
#  Command:
#    name[0](string | NimNode):
#      result[0] = asIdent(name)
#    body[^1](NimNode)
#    head[^2](NimNode):
#      result[^2] = head
#    stuff[1..3](array[3, int]):
#      for i in 1..3:
#        result[i] = newLit(stuff[i-1])
#    arguments[4..^3](varargs[NimNode]):
#      when not isInitialiser:
#        field.del(4, field.len - 3)
#        field.add(children = arguments)
#      else:
#        for i in 0..arguments.high:
#          result[4 + i] = arguments[i]

generate:
  Command:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    body[^1](NimNode)
    head[^2](NimNode)
    stuff[1..3](array[3, int])
    arguments[4..^3](varargs[NimNode])

macro test(): untyped =
  result = newStmtList()
  let testCommand = Command(name = "hello", body = "body", head = "head", stuff = [100, 200, 300], 400, 500)
  echo testCommand.treeRepr
  echo "name: ", testCommand.name.repr
  echo "body: ", testCommand.body.repr
  echo "head: ", testCommand.head.repr
  echo "stuff: ", testCommand.stuff
  echo "arguments: ", testCommand.arguments
  testCommand.name = "goodbye"
  testCommand.body = "set body"
  testCommand.stuff = [101, 202, 303]
  testCommand.arguments = [800, 900]
  echo testCommand.treeRepr


test()

when false:
  proc Command*(name: string | NimNode, arguments: varargs[NimNode]): NimNode =
    const isInitialiser = true
    result = nnkCommand.newTree(toIdent(name))
    when not isInitialiser:
      field.del(1, field.len - 1)
    result.add(children = arguments)
