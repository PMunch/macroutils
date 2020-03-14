import macros, tables#, algorithm

const
  AllNodeKinds* = {NimNodeKind.low..NimNodeKind.high}
  ContainerNodeKinds* = AllNodeKinds - {nnkNone, nnkEmpty, nnkNilLit,
                        nnkCharLit..nnkUInt64Lit, nnkFloatLit..nnkFloat64Lit,
                        nnkStrLit..nnkTripleStrLit}

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

macro massert(x, node: untyped): untyped =
  let textRepr = x.repr
  quote do:
    if not `x`:
      error("Assertion failed: " & `textRepr`, `node`)

macro generate(nodes: untyped, extraFields: untyped): untyped =
  result = newStmtList()
  assert(nodes.kind == nnkStmtList)
  var fields: Table[string, seq[tuple[kind, node: NimNode]]]
  let isInitialiser = newIdentNode("isInitialiser")
  for node in nodes:
    massert(node.kind == nnkCall, node)
    massert(node[0].kind == nnkIdent, node[0])
    massert(node[1].kind == nnkStmtList, node[1])
    var
      positives: seq[BiggestInt]
      negatives: seq[BiggestInt]
      flexible =
        (start: -1.BiggestInt, stop: -1.BiggestInt, node: newEmptyNode())
    for arg in node[1]:
      massert(arg.kind == nnkCall, arg)
      massert(arg[0].kind == nnkBracketExpr, arg[0])
      massert(arg[0][0].kind == nnkIdent, arg[0][0])
      case arg[0][1].kind:
      of nnkIntLit:
        positives.add arg[0][1].intVal
      of nnkInfix:
        massert(arg[0][1][0].kind == nnkIdent, arg[0][1][0])
        case $arg[0][1][0]:
        of "..":
          for i in arg[0][1][1].intVal..arg[0][1][2].intVal:
            positives.add i
        of "..<":
          for i in arg[0][1][1].intVal..<arg[0][1][2].intVal:
            positives.add i
        of "..^":
          flexible =
            (start: arg[0][1][1].intVal, stop: arg[0][1][2].intVal, node: arg)
      of nnkPrefix:
        negatives.add arg[0][1][1].intVal
      else:
        massert(arg[0][1].kind in {nnkIntLit, nnkInfix}, arg[0][1])
    #positives.sort
    #negatives.sort
    for i, v in positives:
      massert(i == v, node)
    if flexible.start != -1:
      massert(positives.len == 0 or positives[^1]+1 == flexible.start, node)
      massert(negatives.len == 0 or negatives[0]+1 == flexible.stop, node)
      massert(positives.len != 0 or flexible.start == 0, node)
      massert(negatives.len != 0 or flexible.stop == 1, node)
    for i, v in negatives:
      massert(negatives.len - i == v, node)
    massert(negatives.len == 0 or flexible.start != -1, node)
    var generator = nnkProcDef.newTree(
      nnkPostfix.newTree(newIdentNode("*"), node[0]),
      newEmptyNode(), newEmptyNode(),
      nnkFormalParams.newTree(newIdentNode("NimNode")),
      newEmptyNode(), newEmptyNode(), newStmtList())
    for arg in node[1]:
      if arg[0][0].strVal != "_":
        generator[3].add nnkIdentDefs.newTree(arg[0][0], arg[1], newEmptyNode())
    let
      nodeKind = newIdentNode("nnk" & $node[0])
      argcount =
        if flexible.start == -1:
          newLit(positives.len)
        else:
          nnkInfix.newTree(newIdentNode("+"),
            newLit(positives.len + negatives.len),
            nnkDotExpr.newTree(flexible.node[0][0], newIdentNode("len")))
    generator[6].add quote do:
      const `isInitialiser` {.used.} = true
      result = newNimNode(`nodeKind`)
      #result.sons = newSeq[NimNode](`argcount`)
      for i in 0..<`argcount`:
        result.add newEmptyNode()
    for arg in node[1]:
      if arg[0][0].strVal == "_":
        echo "skipping"
        continue
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
          raise newException(ValueError, "Unable to get " & `field` &
            " for NimNode of kind " & $`x`.kind)
    var setter = quote do:
      template `setterNameNode`*(`x`: NimNode, `val`: untyped): untyped =
        const `isInitialiser` {.used.} = false
        template result(): untyped {.used.} = `x`
        case `x`.kind:
        else:
          raise newException(ValueError, "Unable to set " & `field` &
            " for NimNode of kind " & $`x`.kind)
    for node in nodes:
      let
        getterBranch =
          nnkOfBranch.newTree(newIdentNode("nnk" & node.kind.strVal))
        setterBranch =
          nnkOfBranch.newTree(newIdentNode("nnk" & node.kind.strVal))
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
            if node.node.len == 3:
              setterBranch.add node.node[2]
            else:
              setterBranch.add quote do:
                `x`.del(`start`, `x`.len - `stop` - `start` + 1)
                for i, v in `val`:
                  `x`.insert(i + `start`, v)
          else:
            getterBranch.add quote do:
              Slice(offset: `start`, length: `length`, node: `x`)
            if node.node.len == 3:
              setterBranch.add node.node[2]
            else:
              setterBranch.add quote do:
                assert `val`.len == `length`, "Unable to set fixed size field to different length: " & `field` & " in node of kind " & $`x`.kind
                for i, v in `val`:
                  `x`[i + `start`] = v
      else: discard
      getter[6][0].insert getter[6][0].len - 1, getterBranch
      setter[6][2].insert setter[6][2].len - 1, setterBranch
    for extra in extraFields:
      massert(extra.kind == nnkCall, extra)
      massert(extra.len == 2, extra)
      massert(extra[0].kind == nnkIdent, extra[0])
      massert(extra[1].kind == nnkIdent, extra[1])
      if extra[1].strVal == field:
        getter[6][0].insert getter[6][0].len - 1, nnkOfBranch.newTree(newIdentNode("nnk" & extra[0].strVal), nnkDotExpr.newTree(x, newIdentNode("strVal")))
        setter[6][2].insert setter[6][2].len - 1, nnkOfBranch.newTree(newIdentNode("nnk" & extra[0].strVal), nnkAsgn.newTree(nnkDotExpr.newTree(x, newIdentNode("strVal")), nameNode))
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
                    uint32, uint64, bool, string, float32, float64, enum,
                    object, tuple)

#converter Lit*[N, T](x: array[N, T]): NimNode = newLit(`x`)
#converter Lit*[T](x: seq[T]): NimNode = newLit(`x`)
#converter Lit*[T](x: set[T]): NimNode = newLit(`x`)

proc asIdent(name: string | NimNode): NimNode =
  when name is NimNode:
    assert name.kind in {nnkIdent, nnkSym},
      "Node must be an identifier or a symbol, but was: " & $name.kind & "(" &
      name.repr & ")"
    name
  else:
    newIdentNode(name)

proc Ident*(name: string): NimNode =
  newIdentNode(name)

proc Sym*(name: static[string]): NimNode =
  bindSym(name)

proc RStrLit*(argument: string): NimNode =
  result = newNimNode(nnkRStrLit)
  result.strVal = argument

proc CommentStmt*(argument: string): NimNode =
  result = nnkCommentStmt.newTree()
  result.strVal = argument

proc BlockStmt*(body: NimNode): NimNode =
  nnkBlockStmt.newTree(newNimNode(nnkEmpty), body)

proc ContinueStmt*(): NimNode =
  newNimNode(nnkContinueStmt)

proc Empty*(): NimNode =
  newEmptyNode()

proc AsmStmt*(body: string | NimNode): NimNode =
  nnkAsmStmt.newTree(newEmptyNode(), body)

proc RaiseStmt*(): NimNode =
  nnkRaiseStmt.newTree(newEmptyNode())

generate:
  Command:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    arguments[1..^1](varargs[NimNode])

  Call:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    arguments[1..^1](varargs[NimNode])

  Infix:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    left[1](NimNode)
    right[2](NimNode)

  Prefix:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    argument[1](NimNode)

  Postfix:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    argument[1](NimNode)

  ExprEqExpr:
    left[0](NimNode)
    right[1](NimNode)

  ExprColonExpr:
    left[0](NimNode)
    right[1](NimNode)

  CallStrLit:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    argument[1](string | NimNode):
      result[1] = when argument is NimNode:
        case argument.kind:
        of nnkRStrLit: argument
        of nnkStrLit: RStrLit(argument.strVal)
        else:
          raise newException(ValueError,
            "Unable to convert NimNode of kind " & $arg.kind & " to nnkRStrLit")
      else:
        RStrLit(argument)

  DerefExpr:
    node[0](NimNode)

  Addr:
    node[0](NimNode)

  Cast:
    bracket[0](NimNode)
    node[1](NimNode)

  DotExpr:
    left[0](NimNode)
    right[1](NimNode)

  BracketExpr:
    node[0](NimNode)
    bracket[1](NimNode)

  Par:
    arguments[0..^1](varargs[NimNode])

  Curly:
    arguments[0..^1](varargs[NimNode])

  Bracket:
    arguments[0..^1](varargs[NimNode])

  TableConstr:
    arguments[0..^1](varargs[NimNode]):
      for i, a in arguments:
        assert a.kind == nnkExprColonExpr,
          "Unable to add non-colon expression to table constructor: " & $a.kind
        result[i] = a

  IfExpr:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to if constructor: " & $a.kind
        result[i] = a

  IfStmt:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to if constructor: " & $a.kind
        result[i] = a

  WhenStmt:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to when constructor: " & $a.kind
        result[i] = a

  ElifExpr:
    cond[0](NimNode)
    body[1](NimNode)

  ElifBranch:
    cond[0](NimNode)
    body[1](NimNode)

  ElseExpr:
    body[0](NimNode)

  Else:
    body[0](NimNode)

  Pragma:
    arguments[0..^1](varargs[NimNode])

  Asgn:
    left[0](NimNode)
    right[1](NimNode)

  StmtList:
    arguments[0..^1](varargs[NimNode])

  CaseStmt:
    cond[0](NimNode)
    branches[1..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkOfBranch, nnkElifBranch, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to case constructor: " & $a.kind
        result[1 + i] = a

  OfBranch:
    arguments[0..^2](openarray[Nimnode])
    body[^1](NimNode)

  WhileStmt:
    cond[0](NimNode)
    body[1](NimNode)

  ForStmt:
    arguments[0..^3](openarray[NimNode])
    iter[^2](NimNode)
    body[^1](NimNode)

  TryStmt:
    body[0](NimNode)
    branches[1..^1](varargs[NimNode]):
      for i, branch in branches:
        assert branch.kind in {nnkExceptBranch, nnkFinally},
          "Unable to add non-except or -finally expression to try " &
          "constructor: " & $branch.kind
        result[1 + i] = branch

  ExceptBranch:
    arguments[0..^2](openarray[NimNode])
    body[^1](NimNode)

  Finally:
    body[0](NimNode)

  ReturnStmt:
    argument[0](NimNode)

  YieldStmt:
    argument[0](NimNode)

  DiscardStmt:
    argument[0](NimNode)

  BreakStmt:
    argument[0](NimNode)

  BlockStmt:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    body[1](NimNode)

  AsmStmt:
    pragmas[0](NimNode)
    body[1](string | NimNode):
      result[1] = when body is string: newLit(body) else: body

  ImportStmt:
    arguments[0..^1](varargs[NimNode])

  ImportExceptStmt:
    left[0](NimNode)
    right[1](NimNode)

  FromStmt:
    left[0](NimNode)
    right[1](NimNode)

  ExportStmt:
    argument[0](NimNode)

  ExportExceptStmt:
    left[0](NimNode)
    right[1](NimNode)

  IncludeStmt:
    arguments[0..^1](varargs[NimNode])

  VarSection:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkIdentDefs,
          "Unable to add something not an ident definition to var section " &
          "constructor: " & $def.kind
        result[i] = def

  LetSection:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkIdentDefs,
          "Unable to add something not an ident definition to let section " &
          "constructor: " & $def.kind
        result[i] = def

  ConstSection:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkConstDef,
          "Unable to add something not an constant definition to const " &
          "section constructor: " & $def.kind
        result[i] = def

  IdentDefs:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    typ[1](NimNode)
    body[2](NimNode)

  ConstDef:
    name[0](NimNode)
    typ[1](NimNode)
    body[2](NimNode)

  Range:
    left[0](NimNode)
    right[1](NimNode)

  AccQuoted:
    arguments[0..^1](varargs[NimNode])

  BindStmt:
    argument[0](NimNode)

  GenericParams:
    definitions[0..^1](varargs[NimNode]):
      for i, def in definitions:
        assert def.kind == nnkIdentDefs,
          "Unable to add something not an ident def to generic parameters " &
          "constructor: " & $def.kind
        result[i] = def

  FormalParams:
    retval[0](NimNode)
    arguments[1..^1](varargs[NimNode])

  OfInherit:
    argument[0](NimNode)

  VarTy:
    argument[0](NimNode)

  ProcDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  FuncDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  IteratorDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  ConverterDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  MethodDef:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  TemplateDef:
    name[0](NimNode)
    terms[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  MacroDef:
    name[0](NimNode)
    terms[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  TypeSection:
    definitions[0..^1](varargs[NimNode])

  TypeDef:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    generics[1](NimNode)
    typ[2](NimNode)

  Defer:
    body[0](NimNode)

  RaiseStmt:
    node[0](NimNode)

  MixinStmt:
    node[0](NimNode)

  BindStmt:
    node[0](NimNode)

  TupleTy:
    node[0](NimNode)

  VarTy:
    node[0](NimNode)

  PtrTy:
    node[0](NimNode)

  RefTy:
    node[0](NimNode)

  DistinctTy:
    node[0](NimNode)

  EnumTy:
    _[0](NimNode)
    definitions[1..^1](varargs[NimNode])

  TypeClassTy:
    arglist[0](NimNode)
    _[1](NimNode)
    _[2](NimNode)
    body[3](NimNode)

  Arglist:
    arguments[0..^1](varargs[NimNode])

  ProcTy:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  IteratorTy:
    name[0](NimNode)
    _[1](NimNode)
    generics[2](NimNode)
    params[3](NimNode)
    pragmas[4](NimNode)
    _[5](NimNode)
    body[6](NimNode)

  StaticTy:
    node[0](NimNode)

  ConstTy:
    node[0](NimNode)

  MutableTy:
    node[0](NimNode)

  SharedTy:
    node[0](NimNode)

  StaticStmt:
    body[0](NimNode)

  UsingStmt:
    definitions[0..^1](varargs[NimNode])

  TypeOfExpr:
    node[0](NimNode)

  ObjectTy:
    pragmas[0](NimNode)
    inherits[1](NimNode)
    body[2](NimNode)

  RecList:
    arguments[0..^1](varargs[NimNode])

  RecCase:
    cond[0](NimNode)
    branches[1..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkOfBranch, nnkElifBranch, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to case constructor: " & $a.kind
        result[1 + i] = a

  RecWhen:
    branches[0..^1](varargs[NimNode]):
      for i, a in branches:
        assert a.kind in {nnkElifBranch, nnkElifExpr, nnkElseExpr, nnkElse},
          "Unable to add non-branch expression to when constructor: " & $a.kind
        result[i] = a

  EnumFieldDef:
    left[0](NimNode)
    right[1](NimNode)

  ObjConstr:
    name[0](string | NimNode):
      result[0] = asIdent(name)
    definitions[1..^1](varargs[NimNode])
do:
  Ident(name)
  Sym(name)
  RStrLit(argument)
  CommentStmt(argument)

template inOrEquals(node: NimNode,
                    nodekind: NimNodeKind or NimNodeKinds): untyped =
  when nodekind is NimNodeKind:
    node.kind == nodekind
  else:
    node.kind in nodekind

proc forNodePos*(node: NimNode,
              kind: NimNodeKind or NimNodeKinds,
              action: proc (x, y: NimNode): NimNode,
              depth, maxDepth: int,
              expr: NimNode = newEmptyNode()): NimNode {.discardable.} =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. The `x` passed to `action` is not the node itself, but
  ## rather it's position in the tree as nested `BrakcketExpr`s. NOTE: This
  ## modifies the original node tree and only returns for easy chaining.
  var node =if node.inOrEquals(kind) and depth <= maxdepth:
      action(node, expr)
    else:
      node

  if node.kind in ContainerNodeKinds and depth < maxdepth:
    result = node
    for i, child in node:
      let newexpr = if expr.kind == nnkEmpty:
        Bracket(Lit i)
      else:
        BracketExpr(expr, Lit i)
      result[i] = forNodePos(child, kind, action, depth + 1, maxdepth, newexpr)
  else:
    result = node

template forNodePos*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  maxdepth: int,
                  action: proc (x, y: NimNode): NimNode): NimNode =
  forNodePos(node, kind, action, 0, maxdepth)

template forNodePos*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  action: proc (x, y: NimNode): NimNode): NimNode =
  forNodePos(node, kind, action, 0, int.high)

proc forNode*(node: NimNode,
              kind: NimNodeKind or NimNodeKinds,
              action: proc (x: NimNode): NimNode,
              depth, maxDepth: int): NimNode {.discardable.} =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. The `x` passed to `action` is the node in the tree.
  ## NOTE: This modifies the original node tree and only returns for easy
  ## chaining.

  # Check if this node is equals, but continue checking within it if it's a
  # container
  var node =
    if node.inOrEquals(kind) and depth <= maxdepth:
      action(node)
    else: node

  # If it's a container, recurse into it, otherwise return it
  if node.kind in ContainerNodeKinds and depth < maxdepth:
    result = node
    for i, child in node:
      result[i] = forNode(child, kind, action, depth + 1, maxdepth)
  else:
    result = node

template forNode*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  maxdepth: int,
                  action: proc (x: NimNode): NimNode): NimNode =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. NOTE: This modifies the original node tree and only
  ## returns for easy chaining. `maxdepth` can be set to stop iterating after a
  ## certain depth of nodes.
  forNode(node, kind, action, 0, maxdepth)

template forNode*(node: NimNode,
                  kind: NimNodeKind or NimNodeKinds,
                  action: proc (x: NimNode): NimNode): NimNode =
  ## Takes a NimNode and a NimNodeKind (or a set of NimNodeKind) and applies
  ## the procedure passed in as `action` to every node that matches the kind
  ## throughout the tree. NOTE: This modifies the original node tree and only
  ## returns for easy chaining.
  forNode(node, kind, action, 0, int.high)

template replaceAll*(node: NimNode,
                     kind: NimNodeKind or NimNodeKinds,
                     replace: NimNode,
                     maxdepth = int.high): NimNode =
  ## Replaces all nodes of `kind` in the tree given by `node` with `replace`
  forNode(node, kind, maxdepth, proc(x: NimNode): NimNode = replace)

template replaceAll*(node: NimNode,
                     find: NimNode,
                     replace: NimNode,
                     maxdepth = int.high): NimNode =
  ## Replaces all nodes that are equal to `find` in the tree given by `node`
  ## with `replace`
  forNode(node, find.kind, maxdepth,
    proc(x: NimNode): NimNode = (if find == x: replace else: x))

proc sameTree*(node: NimNode,
               ignored: NimNodeKind or NimNodeKinds,
               comp: NimNode,
               depth = 0, maxDepth = int.high): bool =
  ## Compares two NimNode trees and verifies that the structure and all kinds
  ## are the same.
  echo "Comparing: ", node.treeRepr, " to: ", comp.treeRepr
  if node.inOrEquals(ignored) or depth >= maxdepth:
    return true
  if node.kind in ContainerNodeKinds:
    result = node.len == comp.len
    for i, child in node:
      if child.inOrEquals(ignored):
        continue
      elif child.kind == comp[i].kind:
        result = result and sameTree(child, ignored, comp[i], depth + 1, maxdepth)
      else:
        result = false
  else:
    result = node.kind == comp.kind

template sameTree*(node: NimNode, comp: NimNode): bool =
  sameTree(node, {}.NimNodeKinds, comp)

template sameTree*(node: NimNode, maxdepth: int, comp: NimNode): bool =
  sameTree(node, {}.NimNodeKinds, comp, 0, maxdepth)

macro superQuote*(x: untyped): untyped =
  ## Converts the input to a `quote` statement, but lifts out the content of
  ## the quoted sections as variables which allows you to do things like:
  ## ..code-block::
  ##   macro someMacro(input: untyped): untyped =
  ##     let x = [newLit(100), newLit(200)]
  ##     result = superQuote do:
  ##       echo `$input[0].name` # If input is a procedure, this is the name of the procedure
  ##       if `x[0]` == 300: # Grabs the first literal
  ##         echo "test"
  ##       elif `x[1]` == 200: # Grabs the second literal
  ##         echo "hello world"

  var
    defs = LetSection()
    body = x.forNode(nnkAccQuoted, proc(x: NimNode): NimNode =
      #x.add Ident "test"
      var str = ""
      for child in x:
        str.add $child
      let sym = genSym()
      defs.add IdentDefs(sym, Empty(), parseExpr(str))
      x.del 0, x.len
      x.add sym
    )
  result = StmtList(defs, Call("quote", body))
  echo result.repr

import sugar, termstyle

macro extract*(ast, pattern: untyped): untyped =
  ## A reverse superQuote macro, takes an AST and a pattern with the same tree
  ## structure but where nodes can be replaced by quoted statements. The macro
  ## will then assign the node in the tree to this statement.
  # TODO: Find way of passing `pattern` to `sameTree`
  result = StmtList()
  var x: seq[tuple[node, pos: NimNode]]
  echo "One"
  echo pattern.treeRepr
  pattern.forNode(nnkStmtList, proc (x: NimNode): NimNode =
    if x.len == 1 and x[0].kind == nnkAccQuoted:
      x[0]
    else:
      x
  )
  #pattern.forNode(ContainerNodeKinds, proc (x: NimNode): NimNode =
  #  echo "Checking: ", x.treeRepr
  #  var quoted: NimNode
  #  for child in x:
  #    if child.kind == nnkAccQuoted:
  #      if quoted.kind == nnkNilLit:
  #        echo "child.kind is nnkAccQuoted, quoted.kind is ", quoted.kind
  #        quoted = child
  #      else:
  #        quoted.reset
  #        break
  #    elif child.kind != nnkEmpty:
  #      quoted.reset
  #      break
  #  if quoted.kind == nnkAccQuoted:
  #    return quoted
  #  else:
  #    return x
  #  #if x.len == 1 and x[0].kind == nnkAccQuoted:
  #  #  return x[0]
  #  #else:
  #  #  return x
  #)
  echo "Two"
  echo pattern.treeRepr
  pattern.forNodePos(nnkAccQuoted, proc (n, y: NimNode): NimNode =
    echo n.treeRepr
    var str = ""
    for child in n:
      str.add $child
    x.add (parseExpr(str), y)
    newNimNode(nnkNilLit) # Insert None node here so they can be ignored later
  )
  echo "Three"
  echo pattern.treeRepr
  result = StmtList(quote do:
    let ptrn = quote do:
      `pattern`
    assert ptrn.sameTree(nnkNilLit, `ast`),
      "Unable to run extract on different trees"
  )
  for y in x:
    let
      innerAst = ast # This is to work around a VM bug
      stmt = y.pos.forNode(nnkBracket, (x) => BracketExpr(innerAst, x[0]))
    if y.node.kind == nnkIdent:
      result.add newLetStmt(y.node, stmt)
    else:
      result.add Asgn(y.node, stmt)
  echo result.repr

macro test(): untyped =
  let testTableConst = TableConstr(ExprColonExpr(newLit("hello"), newLit(100)))
  testTableConst.arguments[0] = ExprColonExpr(newLit("goodbye"), newLit(42))
  testTableConst.arguments[0] = newLit(200)
  echo testTableConst.repr
  let testComment = CommentStmt("Hello world")
  echo testComment.argument
  testComment.argument = "test"
  echo testComment.repr
  let testCommand = Command("testCmd", newLit(100))
  echo testCommand.repr
  echo testCommand.name
  testCommand.name = "echo"
  echo testCommand.repr

#test()

macro test2(input: untyped): untyped =
  var test = quote do:
    echo "Hello world"
    if something:
      echo "test"
  echo red "Start"
  echo test.copyNimTree.forNode(nnkStrLit, 4, (x) => Lit"goodbye").repr
  echo test.copyNimTree.replaceAll(nnkStrLit, Lit"Bob")
           .replaceAll(Sym"echo", Ident"test").repr
  echo test.copyNimTree.forNode({nnkSym, nnkStrLit}, (x) => Ident"test").repr
  echo green test.copyNimTree.replaceAll({nnkSym, nnkStrLit}, Ident"test", 5).repr
  echo blue test.copyNimTree.replaceAll(nnkStrLit, Lit"Bob").sameTree(4, quote do:
    echo "Bob"
    if something:
      echo 100
  )
  echo test.copyNimTree.replaceAll(nnkStrLit, Lit"Bob").sameTree quote do:
    echo "Bob"
  let test2 = quote do:
      echo "Bob"
  echo test.sameTree test2
  let
    test3 = quote do:
      echo "Test"
      block:
        echo "Test"
    test4 = newStmtList quote do:
      echo "test"
  echo test3.treeRepr
  echo test4.treeRepr
  echo test3.sameTree(ignored = nnkBlockStmt, test4)
  let x = [newLit(100), newLit(200)]
  result = superQuote do:
    echo `$input[0].name`
    if `x[0]` == 300:
      echo "test"
    elif `x[1]` == 200:
      echo "hello world"

macro testExtract(input: untyped): untyped =
  var x = newSeq[NimNode](1)
  input.extract do:
    proc `procname`() =
      `x[0]`
  assert procname == Ident("someproc")
  assert x[0] == StmtList(Command(Ident "echo", "Hello world"), Command(Ident "echo", "Hello"))
  result = Command(Ident "echo", "Everything works!")

testExtract:
  proc someproc() =
    echo "Hello world"
    echo "Hello"
#generate:
#  Command:
#    name[0](string | NimNode):
#      result[0] = asIdent(name)
#    body[^1](NimNode)
#    head[^2](NimNode)
#    stuff[1..3](array[3, int])
#    arguments[4..^3](varargs[NimNode])
#
#macro test(): untyped =
#  result = newStmtList()
#  let testCommand = Command(name = "hello", body = "body", head = "head", stuff = [100, 200, 300], 400, 500)
#  echo testCommand.treeRepr
#  echo "name: ", testCommand.name.repr
#  echo "body: ", testCommand.body.repr
#  echo "head: ", testCommand.head.repr
#  echo "stuff: ", testCommand.stuff
#  echo "arguments: ", testCommand.arguments
#  testCommand.name = "goodbye"
#  testCommand.body = "set body"
#  testCommand.stuff = [101, 202, 303]
#  testCommand.arguments = [800, 900]
#  echo testCommand.treeRepr
#
#
#test()

macro testDeclared(): untyped =
  result = newStmtList()
  for kind in NimNodeKind:
    let kindName = newIdentNode($kind)
    let procName = ($kind)[3..^1]
    if procName.len >= 3 and procName[^3..^1] == "Lit":
      result.add quote do:
        declaredNodes.incl `kindName`
    else:
      result.add nnkIfExpr.newTree(nnkElifExpr.newTree(nnkCall.newTree(newIdentNode("declared"), newIdentNode(procName)), quote do:
        declaredNodes.incl `kindName`
      ))
  #echo result.repr

proc check() {.compileTime.} =
  var declaredNodes: set[NimNodeKind]
  testDeclared()
  echo "Declared: ", card(declaredNodes), "/", NimNodeKind.high.int
  echo "Declared nodes: ", declaredNodes
  echo "Undeclared nodes: ", AllNodeKinds - declaredNodes

static:
  check()

when false:
  proc Command*(name: string | NimNode, arguments: varargs[NimNode]): NimNode =
    const isInitialiser = true
    result = nnkCommand.newTree(toIdent(name))
    when not isInitialiser:
      field.del(1, field.len - 1)
    result.add(children = arguments)
