import macros

type
  Slice = object
    offset, length: int
    node: NimNode

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


template len*(s: Slice): untyped = s.length

template addArgs(x: NimNode, arguments: varargs[NimNode]): untyped =
  for a in arguments:
    x.add a

template asIdent(name: string | NimNode): untyped =
  when name is NimNode:
    assert name.kind == nnkIdent, "Node must be an identifier"
    name
  else:
    newIdentNode(name)

template Ident*(name: string): untyped =
  newIdentNode(name)

template Command*(name: string | NimNode, arguments: varargs[NimNode]): untyped =
  var x = nnkCommand.newTree(name.asIdent)
  x.addArgs arguments
  x

template Call*(name: string | NimNode, arguments: varargs[NimNode]): untyped =
  var x = nnkCall.newTree(name.asIdent)
  x.addArgs arguments
  x

template Infix*(name: string | NimNode; left, right: untyped): untyped =
  nnkInfix.newTree(
    name.asIdent,
    left, right)

template Prefix*(name: string | NimNode; arg: untyped): untyped =
  nnkPrefix.newTree(
    name.asIdent,
    arg)

template Postfix*(name: string | NimNode; arg: untyped): untyped =
  nnkPostfix.newTree(
    name.asIdent,
    arg)

template ExprEqExpr*(left, right: string | NimNode): untyped =
  nnkExprEqExpr.newTree(
    left,
    right)

template ExprColonExpr*(left, right: string | NimNode): untyped =
  nnkExprColonExpr.newTree(
    left,
    right)

template RStrLit*(arg: string): untyped =
  var x = newNimNode(nnkRStrLit)
  x.strVal = arg
  x

template CallStrLit*(name, arg: string | NimNode): untyped =
  nnkCallStrLit.newTree(
    name.asIdent,
    when arg is NimNode:
      case arg.kind:
      of nnkRStrLit: arg
      of nnkStrLit: RStrLit(arg.strVal)
      else:
        raise newException(ValueError, "Unable to convert NimNode of kind " & $arg.kind & " to nnkRStrLit")
    else:
      RStrLit(arg)
  )

template DerefExpr*(node: NimNode): untyped =
  nnkDerefExpr.newTree(node)

template Addr*(node: NimNode): untyped =
  nnkAddr.newTree(node)

template Cast*(bracket, node: NimNode): untyped =
  nnkCast.newTree(bracket, node)

template DotExpr*(left, right: NimNode): untyped =
  nnkDotExpr.newTree(left, right)

template BracketExpr*(node, bracket: NimNode): untyped =
  nnkBracketExpr.newTree(node, bracket)

template Par*(args: varargs[NimNode]): untyped =
  var x = nnkPar.newTree()
  x.addArgs(args)
  x

template Curly*(args: varargs[NimNode]): untyped =
  var x = nnkCurly.newTree()
  x.addArgs(args)
  x

template Bracket*(args: varargs[NimNode]): untyped =
  var x = nnkBracket.newTree()
  x.addArgs(args)
  x

template TableConstr*(args: varargs[NimNode]): untyped =
  var x = nnkTableConstr.newTree()
  for a in args:
    assert a.kind == nnkExprColonExpr, "Unable to add non-colon expression to table constructor: " & $a
    x.add a
  x

template IfExpr*(args: varargs[NimNode]): untyped =
  var x = nnkIfExpr.newTree()
  for a in args:
    assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to if constructor: " & $a
    x.add a
  x

template ElifExpr*(cond, body: NimNode): untyped =
  nnkElifExpr.newTree(cond, body)

template ElifBranch*(cond, body: NimNode): untyped =
  nnkElifBranch.newTree(cond, body)

template ElseExpr*(body: NimNode): untyped =
  nnkElseExpr.newTree(body)

template Else*(body: NimNode): untyped =
  nnkElse.newTree(body)

template CommentStmt*(arg: string): untyped =
  var x = nnkCommentStmt.newTree()
  x.strVal = arg
  x

template Pragma*(args: varargs[untyped]): untyped =
  var x = nnkPragma.newTree()
  x.addArgs args
  x

template Asgn*(left, right: NimNode): untyped =
  nnkAsgn.newTree(left, right)

template StmtList*(args: varargs[untyped]): untyped =
  var x = nnkStmtList.newTree()
  x.addArgs args
  x

template CaseStmt*(cond: NimNode, args: varargs[NimNode]): untyped =
  var x = nnkCaseStmt.newTree(cond)
  for a in args:
    assert a.kind in {nnkOfBranch, nnkElifBranch, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to case constructor: " & $a
    x.add a
  x

template OfBranch*(args: openarray[Nimnode], body: NimNode): untyped =
  var x = nnkOfBranch.newTree()
  x.addArgs args
  x.add body
  x

template cond*(x: NimNode): untyped =
  case x.kind:
  of nnkElifExpr, nnkCaseStmt, nnkElifBranch:
    x[0]
  else:
    raise newException(ValueError, "Unable to get condition for NimNode of kind " & $x.kind)

template `cond=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkElifExpr, nnkCaseStmt, nnkElifBranch:
    x[0] = val
  else:
    raise newException(ValueError, "Unable to set condition for NimNode of kind " & $x.kind)

template body*(x: NimNode): untyped =
  case x.kind:
  of nnkElseExpr, nnkElse:
    x[0]
  of nnkElifExpr, nnkElifBranch:
    x[1]
  of nnkOfBranch:
    x[x.len - 1]
  else:
    raise newException(ValueError, "Unable to get body for NimNode of kind " & $x.kind)

template `body=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkElseExpr, nnkElse:
    x[0] = val
  of nnkElifExpr, nnkElifBranch:
    x[1] = val
  of nnkOfBranch:
    x[x.len - 1] = val
  else:
    raise newException(ValueError, "Unable to set body for NimNode of kind " & $x.kind)

template name*(x: NimNode): untyped =
  case x.kind:
  of nnkCommand, nnkCall, nnkInfix, nnkPrefix, nnkPostfix, nnkCallStrLit:
    x[0]
  of nnkIdent:
    x.strVal
  else:
    raise newException(ValueError, "Unable to get name for NimNode of kind " & $x.kind)

template `name=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkCommand, nnkCall, nnkInfix, nnkPrefix, nnkPostfix, nnkCallStrLit:
    x[0] = val
  else:
    raise newException(ValueError, "Unable to set name for NimNode of kind " & $x.kind)

template node*(x: NimNode): untyped =
  case x.kind:
  of nnkDerefExpr, nnkAddr, nnkBracketExpr:
    x[0]
  of nnkCast:
    x[1]
  else:
    raise newException(ValueError, "Unable to get child node for NimNode of kind " & $x.kind)

template `node=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkDerefExpr, nnkAddr, nnkBracketExpr:
    x[0] = val
  of nnkCast:
    x[1] = val
  else:
    raise newException(ValueError, "Unable to set child node for NimNode of kind " & $x.kind)

template bracket*(x: NimNode): untyped =
  case x.kind:
  of nnkCast:
    x[0]
  of nnkBracketExpr:
    x[1]
  else:
    raise newException(ValueError, "Unable to get bracket for NimNode of kind " & $x.kind)

template `bracket=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkCast:
    x[0] = val
  of nnkBracketExpr:
    x[1] = val
  else:
    raise newException(ValueError, "Unable to set bracket for NimNode of kind " & $x.kind)

template args*(x: NimNode): untyped =
  case x.kind:
  of nnkCommand, nnkCall, nnkCaseStmt:
    Slice(offset: 1, length: x.len - 1, node: x)
  of nnkPar, nnkCurly, nnkBracket, nnkTableConstr, nnkIfExpr, nnkPragma, nnkStmtList:
    Slice(offset: 0, length: x.len, node: x)
  of nnkOfBranch:
    Slice(offset: 0, length: x.len - 1, node: x)
  else:
    raise newException(ValueError, "Unable to get arguments for NimNode of kind " & $x.kind)

template `args=`*(x: NimNode, val: openarray[NimNode]): untyped =
  case x.kind:
  of nnkCommand, nnkCall:
    x.del(1, x.len - 1)
    x.add(children = val)
  of nnkPar, nnkCurly, nnkBracket, nnkPragma, nnkStmtList:
    x.del(0, x.len)
    x.add(children = val)
  of nnkTableConstr:
    x.del(0, x.len)
    for child in val:
      assert child.kind == nnkExprColonExpr, "Unable to add non-colon expression to table constructor: " & $child
      x.add child
  of nnkIfExpr:
    x.del(0, x.len)
    for child in val:
      assert child.kind in {nnkElifExpr, nnkElifBranch, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to if constructor: " & $child
      x.add child
  of nnkCaseStmt:
    x.del(1, x.len - 1)
    for child in val:
      assert child.kind in {nnkOfBranch, nnkElifExpr, nnkElifBranch, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to case constructor: " & $child
      x.add child
  of nnkOfBranch:
    x.del(0, x.len - 1)
    for child in val:
      x.insert(0, child)
  else:
    raise newException(ValueError, "Unable to set arguments for NimNode of kind " & $x.kind)

template arg*(x: NimNode): untyped =
  case x.kind:
  of nnkPrefix, nnkPostfix, nnkCallStrLit:
    x[1]
  else:
    raise newException(ValueError, "Unable to get argument for NimNode of kind " & $x.kind)

template `arg=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkPrefix, nnkPostfix, nnkCallStrLit:
    x[1] = val
  else:
    raise newException(ValueError, "Unable to set argument for NimNode of kind " & $x.kind)

template left*(x: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn:
    x[0]
  of nnkInfix:
    x[1]
  else:
    raise newException(ValueError, "Unable to get left element for NimNode of kind " & $x.kind)

template `left=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn:
    x[0] = val
  of nnkInfix:
    x[1] = val
  else:
    raise newException(ValueError, "Unable to get left element for NimNode of kind " & $x.kind)

template right*(x: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn:
    x[1]
  of nnkInfix:
    x[2]
  else:
    raise newException(ValueError, "Unable to get right element for NimNode of kind " & $x.kind)

template `right=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn:
    x[1] = val
  of nnkInfix:
    x[2] = val
  else:
    raise newException(ValueError, "Unable to get right element for NimNode of kind " & $x.kind)

template `[]`*(s: Slice, idx: int): untyped =
  assert idx < s.length, "Index out of bounds"
  s.node[s.offset + idx]

template `[]=`*(s: Slice, idx: int, val: untyped): untyped =
  assert idx < s.length, "Index out of bounds"
  s.node[s.offset + idx] = val

iterator items*(s: Slice): NimNode =
  for i in 0..<s.length:
    yield s.node[s.offset + i]

proc `$`*(s: Slice): string =
  result = "["
  for i in 0..<s.length:
    result.add s.node[s.offset + i].repr
    if i != s.length - 1:
      result.add ", "
  result.add "]"

when isMainModule:
  macro test(): untyped =
    result = BracketExpr(Ident"abc", "def")
    result.bracket = "xyz"
    echo result.repr
    echo result.node
    echo result.bracket
    result = ExprColonExpr("hello", "world")
    echo result.left
    result.right = "Bob"
    echo result.repr
    result = TableConstr(ExprColonExpr(Ident("a"), Lit(3)))
    echo result.repr
    result = Bracket(100, 200, 300)
    echo result.repr
    result = CommentStmt"Hello world"
    echo result.repr
    result = Asgn(Ident"hello", 100)
    result.right = 300
    echo result.repr
    result = IfExpr(
      ElifExpr(Ident"test", Lit"World"),
      Else(Lit"Hello"))
    echo result.repr
    result = CaseStmt(
      Ident"something",
      OfBranch([Lit(100)], Lit"Hello"),
      OfBranch([Lit(100), Lit(200)], Lit"Hello"),
      ElifBranch(Ident"whatever", Lit"World")
    )
    echo result.cond.repr
    echo result.args[2].body
    echo result.repr
    #echo result.name
    #echo result.args.len
    #echo result.args[0]
    #result = Ident("hello")
    #echo result.name
    result = newStmtList()

  test()

  macro testCommand(): untyped =
    result = Command("echo", newLit("Hello world"))
    echo result.treeRepr
    result.args[0] = newLit("Hello there")
    echo result.args.len
    echo result.args[0]
    echo result.treeRepr
    result.args = [newLit("Hello"), newLit("World")]
    echo result.treeRepr
    result.args = [newLit("Hello world")]
    echo result.treeRepr
    result = newStmtList()
  testCommand()

  #macro testCall(): untyped =
  #  result = Call("echo", newLit("Hello world"))
  #  echo result.treeRepr
  #  result.args[0] = newLit("Hello there")
  #  echo result.args.len
  #  echo result.args[0]
  #  echo result.treeRepr

  #testCommand()
  #testCall()
