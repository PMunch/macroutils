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

proc addArgs(x: var NimNode, arguments: varargs[NimNode]) =
  for a in arguments:
    x.add a

proc asIdent(name: string | NimNode): NimNode =
  when name is NimNode:
    assert name.kind == nnkIdent, "Node must be an identifier"
    name
  else:
    newIdentNode(name)

proc Ident*(name: string): NimNode =
  newIdentNode(name)

proc Command*(name: string | NimNode, arguments: varargs[NimNode]): NimNode =
  result = nnkCommand.newTree(name.asIdent)
  result.addArgs arguments

proc Call*(name: string | NimNode, arguments: varargs[NimNode]): NimNode =
  result = nnkCall.newTree(name.asIdent)
  result.addArgs arguments

proc Infix*(name: string | NimNode; left, right: NimNode): NimNode =
  nnkInfix.newTree(
    name.asIdent,
    left, right)

proc Prefix*(name: string | NimNode; arg: NimNode): NimNode =
  nnkPrefix.newTree(
    name.asIdent,
    arg)

proc Postfix*(name: string | NimNode; arg: NimNode): NimNode =
  nnkPostfix.newTree(
    name.asIdent,
    arg)

proc ExprEqExpr*(left, right: string | NimNode): NimNode =
  nnkExprEqExpr.newTree(
    left,
    right)

proc ExprColonExpr*(left, right: string | NimNode): NimNode =
  nnkExprColonExpr.newTree(
    left,
    right)

proc RStrLit*(arg: string): NimNode =
  result = newNimNode(nnkRStrLit)
  result.strVal = arg

proc CallStrLit*(name, arg: string | NimNode): NimNode =
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

proc DerefExpr*(node: NimNode): NimNode =
  nnkDerefExpr.newTree(node)

proc Addr*(node: NimNode): NimNode =
  nnkAddr.newTree(node)

proc Cast*(bracket, node: NimNode): NimNode =
  nnkCast.newTree(bracket, node)

proc DotExpr*(left, right: NimNode): NimNode =
  nnkDotExpr.newTree(left, right)

proc BracketExpr*(node, bracket: NimNode): NimNode =
  nnkBracketExpr.newTree(node, bracket)

proc Par*(args: varargs[NimNode]): NimNode =
  result = nnkPar.newTree()
  result.addArgs(args)

proc Curly*(args: varargs[NimNode]): NimNode =
  result = nnkCurly.newTree()
  result.addArgs(args)

proc Bracket*(args: varargs[NimNode]): NimNode =
  result = nnkBracket.newTree()
  result.addArgs(args)

proc TableConstr*(args: varargs[NimNode]): NimNode =
  result = nnkTableConstr.newTree()
  for a in args:
    assert a.kind == nnkExprColonExpr, "Unable to add non-colon expression to table constructor: " & $a
    result.add a

proc IfExpr*(branches: varargs[NimNode]): NimNode =
  result = nnkIfExpr.newTree()
  for a in branches:
    assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to if constructor: " & $a
    result.add a

proc IfStmt*(branches: varargs[NimNode]): NimNode =
  result = nnkIfStmt.newTree()
  for a in branches:
    assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to if constructor: " & $a
    result.add a

proc WhenStmt*(branches: varargs[NimNode]): NimNode =
  result = nnkWhenStmt.newTree()
  for a in branches:
    assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to if constructor: " & $a
    result.add a

proc ElifExpr*(cond, body: NimNode): NimNode =
  nnkElifExpr.newTree(cond, body)

proc ElifBranch*(cond, body: NimNode): NimNode =
  nnkElifBranch.newTree(cond, body)

proc ElseExpr*(body: NimNode): NimNode =
  nnkElseExpr.newTree(body)

proc Else*(body: NimNode): NimNode =
  nnkElse.newTree(body)

proc CommentStmt*(arg: string): NimNode =
  result = nnkCommentStmt.newTree()
  result.strVal = arg

proc Pragma*(args: varargs[NimNode]): NimNode =
  result = nnkPragma.newTree()
  result.addArgs args

proc Asgn*(left, right: NimNode): NimNode =
  nnkAsgn.newTree(left, right)

proc StmtList*(args: varargs[NimNode]): NimNode =
  result = nnkStmtList.newTree()
  result.addArgs args

proc CaseStmt*(cond: NimNode, branches: varargs[NimNode]): NimNode =
  result = nnkCaseStmt.newTree(cond)
  for a in branches:
    assert a.kind in {nnkOfBranch, nnkElifBranch, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to case constructor: " & $a
    result.add a

proc OfBranch*(args: openarray[Nimnode], body: NimNode): NimNode =
  result = nnkOfBranch.newTree()
  result.addArgs args
  result.add body

proc WhileStmt*(cond, body: NimNode): NimNode =
  nnkWhileStmt.newTree(cond, body)

proc ForStmt*(args: openarray[NimNode], iter, body: NimNode): NimNode =
  result = nnkForStmt.newTree()
  result.addArgs args
  result.add iter
  result.add body

proc TryStmt*(body: NimNode, branches: varargs[NimNode]): NimNode =
  result = nnkTryStmt.newTree(body)
  for branch in branches:
    assert branch.kind in {nnkExceptBranch, nnkFinally}, "Unable to add non-except or -finally expression to try constructor: " & $branch
    result.add branch

proc ExceptBranch*(args: openarray[NimNode], body: NimNode): NimNode =
  result = nnkExceptBranch.newTree()
  result.addArgs args
  result.add body

proc Finally*(body: NimNode): NimNode =
  nnkFinally.newTree(body)

proc ReturnStmt*(arg: NimNode): NimNode =
  nnkReturnStmt.newTree(arg)

proc YieldStmt*(arg: NimNode): NimNode =
  nnkYieldStmt.newTree(arg)

proc DiscardStmt*(arg: NimNode): NimNode =
  nnkDiscardStmt.newTree(arg)

proc BreakStmt*(arg: NimNode): NimNode =
  nnkBreakStmt.newTree(arg)

proc BlockStmt*(name: string | NimNode, body: NimNode): NimNode =
  nnkBlockStmt.newTree(name.asIdent, body)

proc BlockStmt*(body: NimNode): NimNode =
  nnkBlockStmt.newTree(newNimNode(nnkEmpty), body)

proc ContinueStmt*(): NimNode =
  newNimNode(nnkContinueStmt)

proc AsmStmt*(pragmas, body: string | NimNode): NimNode =
  when body is string:
    nnkAsmStmt.newTree(pragmas, newLit(body))
  else:
    nnkAsmStmt.newTree(pragmas, body)

proc AsmStmt*(body: string | NimNode): NimNode =
  AsmStmt(newNimNode(nnkEmpty), body)

proc ImportStmt*(args: varargs[NimNode]): NimNode =
  result = nnkImportStmt.newTree()
  result.addArgs args

proc ImportExceptStmt*(left, right: NimNode): NimNode =
  nnkImportExceptStmt.newTree(left, right)

proc FromStmt*(left, right: NimNode): NimNode =
  nnkFromStmt.newTree(left, right)

proc ExportStmt*(arg: NimNode): NimNode =
  nnkExportStmt.newTree(arg)

proc ExportExceptStmt*(left, right: NimNode): NimNode =
  nnkExportExceptStmt.newTree(left, right)

proc IncludeStmt*(args: varargs[NimNode]): NimNode =
  result = nnkIncludeStmt.newTree()
  result.addArgs args

proc VarSection*(defs: varargs[NimNode]): NimNode =
  result = nnkVarSection.newTree()
  for def in defs:
    assert def.kind == nnkIdentDefs, "Unable to add something not an ident definition to var section constructor: " & $def
    result.add def

proc LetSection*(defs: varargs[NimNode]): NimNode =
  result = nnkLetSection.newTree()
  for def in defs:
    assert def.kind == nnkIdentDefs, "Unable to add something not an ident definition to let section constructor: " & $def
    result.add def

proc ConstSection*(defs: varargs[NimNode]): NimNode =
  result = nnkConstSection.newTree()
  for def in defs:
    assert def.kind == nnkConstDef, "Unable to add something not a constant definition to const section constructor: " & $def
    result.add def

proc IdentDefs*(name, typ, body: NimNode): NimNode =
  nnkIdentDefs.newTree(name, typ, body)

proc ConstDef*(name, typ, body: NimNode): NimNode =
  nnkConstDef.newTree(name, typ, body)

template cond*(x: NimNode): untyped =
  case x.kind:
  of nnkElifExpr, nnkCaseStmt, nnkElifBranch, nnkWhileStmt:
    x[0]
  else:
    raise newException(ValueError, "Unable to get condition for NimNode of kind " & $x.kind)

template `cond=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkElifExpr, nnkCaseStmt, nnkElifBranch, nnkWhileStmt:
    x[0] = val
  else:
    raise newException(ValueError, "Unable to set condition for NimNode of kind " & $x.kind)

template iter*(x: NimNode): untyped =
  case x.kind:
  of nnkForStmt:
    x[x.len - 2]
  else:
    raise newException(ValueError, "Unable to get iterator for NimNode of kind " & $x.kind)

template `iter=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkForStmt:
    x[x.len - 2] = val
  else:
    raise newException(ValueError, "Unable to set iterator for NimNode of kind " & $x.kind)

template pragmas*(x: NimNode): untyped =
  case x.kind:
  of nnkAsmStmt:
    x[0]
  else:
    raise newException(ValueError, "Unable to get pragmas for NimNode of kind " & $x.kind)

template `pragmas=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkAsmStmt:
    x[0] = val
  else:
    raise newException(ValueError, "Unable to set pragmas for NimNode of kind " & $x.kind)

template body*(x: NimNode): untyped =
  case x.kind:
  of nnkElseExpr, nnkElse, nnkTryStmt, nnkFinally:
    x[0]
  of nnkElifExpr, nnkElifBranch, nnkWhileStmt, nnkBlockStmt:
    x[1]
  of nnkOfBranch, nnkForStmt, nnkExceptBranch, nnkAsmStmt, nnkIdentDefs, nnkConstDef:
    x[x.len - 1]
  else:
    raise newException(ValueError, "Unable to get body for NimNode of kind " & $x.kind)

template `body=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkElseExpr, nnkElse, nnkTryStmt, nnkFinally:
    x[0] = val
  of nnkElifExpr, nnkElifBranch, nnkWhileStmt, nnkBlockStmt:
    x[1] = val
  of nnkOfBranch, nnkForStmt, nnkExceptBranch, nnkAsmStmt, nnkIdentDefs, nnkConstDef:
    x[x.len - 1] = val
  else:
    raise newException(ValueError, "Unable to set body for NimNode of kind " & $x.kind)

template typ*(x: NimNode): untyped =
  case x.kind:
  of nnkIdentDefs, nnkConstDef:
    x[1]
  else:
    raise newException(ValueError, "Unable to get kind for NimNode of kind " & $x.kind)

template `typ=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkIdentDefs, nnkConstDef:
    x[0] = val
  else:
    raise newException(ValueError, "Unable to set kind for NimNode of kind " & $x.kind)

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

template branches*(x: NimNode): untyped =
  case x.kind:
  of nnkCaseStmt, nnkTryStmt:
    Slice(offset: 1, length: x.len - 1, node: x)
  of nnkIfStmt, nnkIfExpr, nnkWhenStmt:
    Slice(offset: 0, length: x.len, node: x)
  else:
    raise newException(ValueError, "Unable to get branches for NimNode of kind " & $x.kind)

template `branches=`*(x: NimNode, val: openarray[NimNode]): untyped =
  case x.kind:
  of nnkTryStmt:
    x.del(1, x.len - 1)
    for child in val:
      assert child.kind in {nnkExceptBranch, nnkFinally}, "Unable to add non-except or -finally expression to try constructor: " & $child
      x.add child
  of nnkIfExpr, nnkIfStmt, nnkWhenStmt:
    x.del(0, x.len)
    for child in val:
      assert child.kind in {nnkElifExpr, nnkElifBranch, nnkElseExpr, nnkElse},
        "Unable to add non-branch expression to " & (if x.kind == nnkWhenStmt: "when" else: "if") & " constructor: " & $child
      x.add child
  of nnkCaseStmt:
    x.del(1, x.len - 1)
    for child in val:
      assert child.kind in {nnkOfBranch, nnkElifExpr, nnkElifBranch, nnkElseExpr, nnkElse}, "Unable to add non-branch expression to case constructor: " & $child
      x.add child
  else:
    raise newException(ValueError, "Unable to set branches for NimNode of kind " & $x.kind)

template defs*(x: NimNode): untyped =
  case x.kind:
  of nnkLetSection, nnkVarSection, nnkConstSection:
    Slice(offset: 0, length: x.len, node: x)
  else:
    raise newException(ValueError, "Unable to get definitions for NimNode of kind " & $x.kind)

template `defs=`*(x: NimNode, val: openarray[NimNode]): untyped =
  case x.kind:
  of nnkVarSection, nnkLetSection:
    x.del(0, x.len)
    for child in val:
      assert def.kind == nnkIdentDefs,
        "Unable to add something not an ident definition to " & (if x.kind == nnkVarSection: "var" else: "let") & " section constructor: " & $def
      x.add child
  of nnkConstSection:
    x.del(0, x.len)
    for child in val:
      assert def.kind == nnkConstDef,
        "Unable to add something not a constant definition to const section constructor: " & $def
      x.add child
  else:
    raise newException(ValueError, "Unable to set branches for NimNode of kind " & $x.kind)
template args*(x: NimNode): untyped =
  case x.kind:
  of nnkCommand, nnkCall:
    Slice(offset: 1, length: x.len - 1, node: x)
  of nnkPar, nnkCurly, nnkBracket, nnkTableConstr, nnkPragma, nnkStmtList, nnkImportStmt, nnkIncludeStmt:
    Slice(offset: 0, length: x.len, node: x)
  of nnkOfBranch, nnkExceptBranch:
    Slice(offset: 0, length: x.len - 1, node: x)
  of nnkForStmt:
    Slice(offset: 0, length: x.len - 2, node: x)
  else:
    raise newException(ValueError, "Unable to get arguments for NimNode of kind " & $x.kind)

template `args=`*(x: NimNode, val: openarray[NimNode]): untyped =
  case x.kind:
  of nnkCommand, nnkCall:
    x.del(1, x.len - 1)
    x.add(children = val)
  of nnkPar, nnkCurly, nnkBracket, nnkPragma, nnkStmtList, nnkImportStmt, nnkIncludeStmt:
    x.del(0, x.len)
    x.add(children = val)
  of nnkTableConstr:
    x.del(0, x.len)
    for child in val:
      assert child.kind == nnkExprColonExpr, "Unable to add non-colon expression to table constructor: " & $child
      x.add child
  of nnkOfBranch, nnkExceptBranch:
    x.del(0, x.len - 1)
    for child in val:
      x.insert(0, child)
  of nnkForStmt:
    x.del(0, x.len - 2)
    for child in val:
      x.insert(0, child)
  else:
    raise newException(ValueError, "Unable to set arguments for NimNode of kind " & $x.kind)

template arg*(x: NimNode): untyped =
  case x.kind:
  of nnkReturnStmt, nnkYieldStmt, nnkDiscardStmt, nnkBreakStmt, nnkExportStmt:
    x[0]
  of nnkPrefix, nnkPostfix, nnkCallStrLit:
    x[1]
  else:
    raise newException(ValueError, "Unable to get argument for NimNode of kind " & $x.kind)

template `arg=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkReturnStmt, nnkYieldStmt, nnkDiscardStmt, nnkBreakStmt, nnkExportStmt:
    x[0] = val
  of nnkPrefix, nnkPostfix, nnkCallStrLit:
    x[1] = val
  else:
    raise newException(ValueError, "Unable to set argument for NimNode of kind " & $x.kind)

template left*(x: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn, nnkImportExceptStmt, nnkFromStmt, nnkExportExceptStmt:
    x[0]
  of nnkInfix:
    x[1]
  else:
    raise newException(ValueError, "Unable to get left element for NimNode of kind " & $x.kind)

template `left=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn, nnkImportExceptStmt, nnkFromStmt, nnkExportExceptStmt:
    x[0] = val
  of nnkInfix:
    x[1] = val
  else:
    raise newException(ValueError, "Unable to get left element for NimNode of kind " & $x.kind)

template right*(x: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn, nnkImportExceptStmt, nnkFromStmt, nnkExportExceptStmt:
    x[1]
  of nnkInfix:
    x[2]
  else:
    raise newException(ValueError, "Unable to get right element for NimNode of kind " & $x.kind)

template `right=`*(x, val: NimNode): untyped =
  case x.kind:
  of nnkDotExpr, nnkExprEqExpr, nnkExprColonExpr, nnkAsgn, nnkImportExceptStmt, nnkFromStmt, nnkExportExceptStmt:
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
    echo result.cond.repr # Echos out "something"
    echo result.branches[2].body # This is the body of the elif branch, "World"
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
