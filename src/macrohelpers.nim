
import macros
import ast_pattern_matching
import options
import optionsutils
import strformat
import sequtils

export options
export macros


func newBreakStmt*(): NimNode =
    nnkBreakStmt.newTree(newEmptyNode())

func newBreakStmt*(label: NimNode): NimNode =
    label.expectKind(nnkIdent)
    nnkBreakStmt.newTree(label)

func newBracketExpr*(a: NimNode, b: varargs[NimNode]): NimNode =
    nnkBracketExpr.newTree(a).add(b)

func newIndex*(a: NimNode, b: int): NimNode =
    a.newBracketExpr(newIntLitNode(b))

func add*(father: NimNode, child: Option[NimNode]): NimNode =
    withSome child:
        some child:
            father.add child
        none:
            father

func add*(father: NimNode, children: Option[seq[NimNode]]): NimNode =
    withSome children:
        some children:
            father.add children
        none:
            father

func add*(father: NimNode, children: seq[Option[NimNode]]): NimNode =
    for child in children:
        withSome child:
            some child:
                father.add child
            none:
                discard
    father

func addElse*(ifStmt: NimNode, elseBranch: NimNode): NimNode =
    ## When `ifStmt` has no else branches, add to `elseBranch` `ifStmt`
    ifStmt.expectKind({nnkIfStmt, nnkIfExpr})
    elseBranch.expectKind({nnkElse, nnkElseExpr})
    if ifStmt.len == 0 or ifStmt[^1].kind notin {nnkElse, nnkElseExpr}:
        ifStmt.add elseBranch
    ifStmt

func infix*(lhs, op, rhs: NimNode): NimNode =
    nnkInfix.newTree(op, lhs, rhs)

func newDiscard(exp: NimNode = newEmptyNode()): NimNode =
    nnkDiscardStmt.newTree(exp)

macro error2*(msg: static[string], n: untyped): untyped =
    ## macro version of `error` in macros
    ## This is useful for use in template
    error msg, n
macro hint2*(msg: static[string], n: untyped): untyped =
    ## macro version of `hint` in macros
    ## This is useful for use in template
    hint msg, n
macro warning2*(msg: static[string], n: untyped): untyped =
    ## macro version of `warning` in macros
    ## This is useful for use in template
    warning msg, n

macro expandMacros2*(body: untyped): untyped =
    ## for untyped body
    echo body.toStrLit
    body

type
    ParamKind = enum
        Lit
        Expr

macro build*(a: untyped): untyped =
    result = a

proc regenSymImpl(n: NimNode, name: string, sym: NimNode): NimNode =
    result = n
    for i in 0..<n.len:
        if result[i].kind == nnkSym and result[i].strVal == name:
            result[i] = sym
            continue
        result[i] = result[i].regenSymImpl(name, sym)
proc regenSym*(n: NimNode, name: string, kind: NimSymKind = nskLet): NimNode =
    let sym = kind.genSym name
    n.regenSymImpl(name, sym)

template defOfType*(ty: type): untyped =
    template `of ty`(kind: typed): untyped =
        macro impl(k: static[ty]): untyped =
            discard
        when compiles(impl(kind)):
            impl(kind)
        else:
            const s = kind.astToStr & " is not of type " & $ty
            error2 s, kind

defOfType(ParamKind)

macro checkPattern(kind: untyped): untyped =
    result = newStmtList()
    kind.matchAst:
    of nnkExprColonExpr(_, `kind`@_):
        result.add newCall(bindSym"ofParamKind", kind)
    of nnkPrefix(ident"@", nnkBracket(`p`@nnkExprColonExpr(_, `kind`@_))):
        # result.add newCall(bindSym"checkPattern", p)
        result.add newCall(bindSym"ofParamKind", kind)
    else:
        error "invalid pattern", kind
    echo result.repr
    echo result.treeRepr
    

macro Macro*(name: untyped, branches: varargs[untyped]): untyped =
    result = newStmtList()
    result.add newProc(name, @[bindSym"untyped"], newStmtList(newDiscard()), nnkMacroDef)
    for branch in branches:
        branch.expectKind(nnkOfBranch)
        result.add newBlockStmt( # 
            ident"why is it needed?",
            newCall(bindSym"checkPattern", branch[0][0])
        )
    echo result.treeRepr
    echo result.repr

Macro echoLiteral:
of (n: Expr):
    echo n
of (n: Expr):
    echo n
of (@[n: Lit]):
    @@[echoLiteral n]
