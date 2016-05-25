namespace Neo4jClient.FsQuotations

open System
open System.Reflection
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers
open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.Patterns
open Neo4jClient.Cypher

[<AutoOpen>]
module internal QuotationsHelpers =

    #if INTERACTIVE
    let debug format = Printf.kprintf (printfn "[Debug] %s") format
    #else
    let debug format = Printf.kprintf ignore format
    #endif

    let fsQuotationAssembly = matchNode.GetType().DeclaringType.Assembly

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type NodeExpr =
        | Var of var:Var
        // TODO denisok: this representation is totally impractical: total loss of information
        | NamedValue of cypherExpr:string * paramName:string * value:obj
        | Anonymous of typ:Type
        member x.CypherExprForCreate =
            match x with
            | Var(var) -> sprintf "(%s)" var.Name
            | NamedValue(cypherExpr, _, _) -> cypherExpr
            | Anonymous(typ) -> sprintf "(:%s)" typ.Name
        member x.CypherExpr =
            match x with
            | Var(var) -> sprintf "(%s:%s)" var.Name var.Type.Name
            | NamedValue(cypherExpr, _, _) -> cypherExpr
            | Anonymous(typ) -> sprintf "(:%s)" typ.Name
        member x.Parameter =
            match x with
            | Var(_var) -> None
            | NamedValue(_, paramName, value) -> Some (paramName, value)
            | Anonymous(_) -> None

        member x.Create(cypher: ICypherFluentQuery) =
            match x with
            | Var(_var) -> cypher.Create(x.CypherExpr)
            | NamedValue(cypherExpr, paramName, value) ->
                cypher.Create(cypherExpr).WithParam(paramName, value)
            | Anonymous(_) -> failwith "Can't create anonymous nodes"

        member x.Merge(cypher: ICypherFluentQuery) =
            match x with
            | NamedValue(_cypherExpr, paramName, value) ->
                match NodeExpr.TryFindNeo4jKey value with
                | Some (keyName, keyValue) ->
                    let mergeExpr =
                        sprintf
                            "(%s:%s {%s: %A})"
                            paramName (value.GetType().Name) keyName keyValue

                    cypher.Merge(mergeExpr)
                          .OnCreate()
                          .Set(sprintf "%s = {__arg}" paramName)
                          .WithParam("__arg", value)
                | None -> failwith "Value should have a unique Neo4jKey attribute"

            | Var(_) -> failwith "Can't merge node whose value is unknown before running the query"
            | Anonymous(_) -> failwith "Can't merge anonymous nodes"

        static member private TryFindNeo4jKey (entity: obj) =
            let properties = entity.GetType().GetProperties()
            properties
            |> Array.tryPick (fun prop ->
                prop.CustomAttributes
                |> Seq.tryPick (fun attr ->
                    if attr.AttributeType = typeof<Neo4jKeyAttribute> then
                        Some (prop.Name, prop.GetValue(entity))
                    else
                        None
                )
            )

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type RelExpr =
        | Var of var:Var
        | NamedValue of name:string * typ:Type * value:obj
        | TypedPattern of cypherExpr:string

        static member GenParamName name = sprintf "__neo4jrel__%s" name

        member x.CypherExprForCreate =
            match x with
            | Var(_) -> failwith "Can't create anonymous relation"
            | NamedValue(name, typ, _value) ->
                sprintf "[%s:%s {%s}]" name typ.Name (RelExpr.GenParamName name)
            | TypedPattern(cypherExpr) -> cypherExpr

        member x.CypherExpr =
            match x with
            | Var(var) -> sprintf "[%s:%s]" var.Name var.Type.Name
            | NamedValue(name,typ,_) -> sprintf "[%s:%s]" name typ.Name
            | TypedPattern(cypherExpr) -> cypherExpr

        member x.Parameter =
            match x with
            | NamedValue(name, typ, value) -> Some (RelExpr.GenParamName name, value)
            | Var _
            | TypedPattern _ -> None

        member x.Create(cypher: ICypherFluentQuery) =
            match x with
            | NamedValue(name, typ, value) ->
                let paramName = RelExpr.GenParamName name
                cypher.Create(x.CypherExprForCreate).WithParam(paramName, value)
            | TypedPattern(cypherExpr) -> cypher.Create(cypherExpr)
            | Var _ -> cypher.Create(x.CypherExprForCreate)

    [<RequireQualifiedAccess>]
    [<NoComparison>]
    type CypherPattern =
        | LeftRel of leftNode:NodeExpr * rel:RelExpr * rightNode:NodeExpr * isUnique:bool // TODO denisok: isUnique should be extracted to another concept
        | RightRel of leftNode:NodeExpr * rel:RelExpr * rightNode:NodeExpr * isUnique:bool // TODO denisok: isUnique should be extracted to another concept
        | LeftOrRightRel of leftNode:NodeExpr * rel:RelExpr * rightNode:NodeExpr
        member x.CypherExprForCreate =
            match x with
            | LeftRel(lnode,rel,rnode,_) ->
                sprintf "%s<-%s-%s" lnode.CypherExprForCreate rel.CypherExprForCreate rnode.CypherExprForCreate
            | RightRel(lnode,rel,rnode,_) ->
                sprintf "%s-%s->%s" lnode.CypherExprForCreate rel.CypherExprForCreate rnode.CypherExprForCreate
            | LeftOrRightRel(lnode,rel,rnode) ->
                sprintf "%s-%s-%s" lnode.CypherExprForCreate rel.CypherExprForCreate rnode.CypherExprForCreate

        member x.CypherExpr =
            match x with
            | LeftRel(lnode, rel, rnode,_) ->
                sprintf "%s<-%s-%s" lnode.CypherExpr rel.CypherExpr rnode.CypherExpr
            | RightRel(lnode, rel, rnode,_) ->
                sprintf "%s-%s->%s" lnode.CypherExpr rel.CypherExpr rnode.CypherExpr
            | LeftOrRightRel(lnode, rel, rnode) ->
                sprintf "%s-%s-%s" lnode.CypherExpr rel.CypherExpr rnode.CypherExpr

        member x.Create(cypher: ICypherFluentQuery) =
            match x with
            | LeftRel(lnode,rel,rnode,isUnique)
            | RightRel(lnode,rel,rnode,isUnique) ->
                let tryFold opt f arg =
                    match opt with
                    | Some value -> f arg value
                    | None -> arg

                let withParamFolder (cypher: ICypherFluentQuery) (paramName, value) =
                    cypher.WithParam(paramName, value)

                debug "Create relation: %s" x.CypherExprForCreate

                if isUnique then
                    cypher.CreateUnique(x.CypherExprForCreate)
                else
                    cypher.Create(x.CypherExprForCreate)
                |> tryFold lnode.Parameter withParamFolder
                |> tryFold rnode.Parameter withParamFolder
                |> tryFold rel.Parameter withParamFolder
            | _ -> failwith (sprintf "Can create this type of pattern: %A" x)

        member x.Merge(cypher: ICypherFluentQuery) =
            match x with
            | LeftRel(_, RelExpr.NamedValue(relName, _, relValue), _, _)
            | RightRel(_, RelExpr.NamedValue(relName, _, relValue), _, _) ->
                let paramName = RelExpr.GenParamName relName
                cypher.Merge(x.CypherExpr)
                      .OnCreate()
                      .Set(sprintf "%s = {%s}" relName paramName)
                      .WithParam(paramName, relValue)
            | _ -> failwith (sprintf "Can MERGE this type of pattern: %A" x)

    let inline (|IsPredefinedMethod|_|) name (methodInfo: MethodInfo) =
        if methodInfo.Name = name &&
           methodInfo.DeclaringType.Assembly = fsQuotationAssembly
        then Some ()
        else None

    let inline (|DeclareNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ declareNode @> (_, [nodeType], _) -> Some nodeType
        | _ -> None

    let inline (|DeclareRelationshipCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ declareRelationship @> (_, [relType], _) -> Some relType
        | _ -> None

    let (|PropertyGetChain|_|) (expr: Expr) =
        let rec impl (expr: Expr) =
            match expr with
            | PropertyGet(Some (PropertyGet _ as propChain), propInfo, []) ->
                impl propChain
                |> Option.map (fun (entity, chain) -> entity, propInfo.Name :: chain)
            | PropertyGet(Some (Var instance), propInfo, []) ->
                Some (instance, [propInfo.Name])
            | _ -> None

        impl expr
        |> Option.map (fun (entity, propChain) -> entity, List.rev propChain)

    let inline (|IsNodeExpr|_|) expr: Option<NodeExpr> =
        match expr with
        | Var(var) -> Some (NodeExpr.Var(var))
        | ValueWithName(value, typ, name)
        | Coerce(ValueWithName(value, typ, name), _) ->
            // TODO denisok: extract constant
            let paramName = sprintf "__neo4jnode__%s" name
            let cypherExpr = sprintf "(%s:%s {%s})" name typ.Name paramName
            Some (NodeExpr.NamedValue(cypherExpr, paramName, value))
        | DeclareNodeCall(nodeType) -> Some (NodeExpr.Anonymous(nodeType))
        | PropertyGet(None, propInfo, []) -> Some (NodeExpr.NamedValue("", propInfo.Name, propInfo.GetValue(null)))
        | _ ->
            debug "%O" expr
            None

    let inline (|IsRelExpr|_|) expr: Option<RelExpr> =
        match expr with
        | Var(var) -> Some (RelExpr.Var(var))
        | ValueWithName(value, typ, name) -> Some (RelExpr.NamedValue(name, typ, value))
        | DeclareRelationshipCall typ ->
            let cypherExpr = sprintf "[:%s]" typ.Name
            Some (RelExpr.TypedPattern(cypherExpr))
        | _ ->
            debug "%O" expr
            None

    let inline (|CreateNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ createNode @>
            (_, _, [IsNodeExpr(NodeExpr.NamedValue _ as nodeExpr)]) ->
            debug "Create node: %A" nodeExpr
            Some nodeExpr
        | _ -> None

    let inline (|MergeNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ mergeNodeByKey @>
            (_, _, [IsNodeExpr(NodeExpr.NamedValue _ as nodeExpr)]) ->
            debug "Merge node: %A" nodeExpr
            Some nodeExpr
        | _ -> None

    let inline (|MatchNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ matchNode @> (_, _, [nodeArg]) -> Some nodeArg
        | _ -> None

    let (|IsMatchRelationClause|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ matchRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.LeftOrRightRel(nodeExpr1, relExpr, nodeExpr2))
        | SpecificCall <@ matchRightRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.RightRel(nodeExpr1, relExpr, nodeExpr2, false))
        | SpecificCall <@ matchLeftRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.LeftRel(nodeExpr1, relExpr, nodeExpr2, false))
        | _ -> None

    let inline (|CreateRelationCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ createRightRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.RightRel(nodeExpr1, relExpr, nodeExpr2, isUnique=false))

        | SpecificCall <@ createUniqueRightRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.RightRel(nodeExpr1, relExpr, nodeExpr2, isUnique=true))

        | SpecificCall <@ createLeftRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.LeftRel(nodeExpr1, relExpr, nodeExpr2, isUnique=false))

        | SpecificCall <@ createUniqueLeftRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.LeftRel(nodeExpr1, relExpr, nodeExpr2, isUnique=true))

        | _ -> None

    let inline (|MergeRelationCall|_|) expr =
        match expr with
        | SpecificCall <@ mergeRightRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.RightRel(nodeExpr1, relExpr, nodeExpr2, isUnique=false))
        | SpecificCall <@ mergeLeftRelation @> (_, _, [IsNodeExpr(nodeExpr1); IsRelExpr(relExpr); IsNodeExpr(nodeExpr2)]) ->
            Some (CypherPattern.LeftRel(nodeExpr1, relExpr, nodeExpr2, isUnique=false))
        | _ -> None

    let inline (|DeleteNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ deleteNode @> (_, _, [node]) -> Some node
        | _ -> None

    let inline (|DeleteRelationshipCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ deleteRelationship @> (_, _, [rel]) -> Some rel
        | _ -> None

    let inline (|WhereCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ where @> (_, _, [arg]) -> Some arg
        | _ -> None

    let inline (|ReturnResultsCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ returnResults @> (_,_,[returnArg]) -> Some returnArg
        | _ -> None

    let (|IsOperand|_|) (expr: Expr) =
        match expr with
        | PropertyGetChain(entity, propChain) ->
            [|
                yield entity.Name
                yield! propChain
            |]
            |> fun strings -> Some (String.Join(".", strings))
        | Value(value, _typ) ->
            match value with
            | :? string -> Some (sprintf "\"%O\"" value)
            | :? int | :? bool | :? float
                // TODO denisok: more types to support
                -> Some (string value)
            | _ -> None
        | _ -> None

    [<RequireQualifiedAccess; NoComparison>]
    type PredicateExpr =
        | PrefixUnary of op:string * expr:string
        | PostfixUnary of op:string * expr:string
        | BinaryComp of op:string * left:string * right:string
        | And of left:PredicateExpr * right:PredicateExpr
        | Or of left:PredicateExpr * right:PredicateExpr
        | Not of expr:PredicateExpr
        member x.CypherExpr =
            match x with
            | BinaryComp(op, left, right) -> sprintf "%O %s %O" left op right
            | And(left, right) -> sprintf "(%s) AND (%s)" left.CypherExpr right.CypherExpr
            | Or(left, right) -> sprintf "(%s) OR (%s)" left.CypherExpr right.CypherExpr
            | Not e -> sprintf "NOT (%s)" e.CypherExpr
            | PrefixUnary(op, expr) -> sprintf "%s %O" op expr
            | PostfixUnary(op, expr) -> sprintf "%O %s" expr op

    let (|IsUnaryExpr|_|) (expr: Expr) =
        match expr with
        | SpecificCall <@ isNull @> (_, _, [IsOperand(arg)]) -> Some (PredicateExpr.PostfixUnary("IS NULL", arg))
        | SpecificCall <@ isNotNull @> (_, _, [IsOperand(arg)]) -> Some (PredicateExpr.PostfixUnary("IS NOT NULL", arg))
        | _ -> None

    let (|IsBinaryExpr|_|) (expr: Expr) =
        match expr with
        // NOTE: refactoring the pattern matches lead to exponential type inference/compile time
        | SpecificCall <@ (=) @> (_, _, [IsOperand(arg1); IsOperand(arg2)]) -> Some (PredicateExpr.BinaryComp("=", arg1, arg2))
        | SpecificCall <@ (<>) @> (_, _, [IsOperand(arg1); IsOperand(arg2)]) -> Some (PredicateExpr.BinaryComp("<>", arg1, arg2))
        | SpecificCall <@ (<) @> (_, _, [IsOperand(arg1); IsOperand(arg2)]) -> Some (PredicateExpr.BinaryComp("<", arg1, arg2))
        | SpecificCall <@ (>) @> (_, _, [IsOperand(arg1); IsOperand(arg2)]) -> Some (PredicateExpr.BinaryComp(">", arg1, arg2))
        | SpecificCall <@ (<=) @> (_, _, [IsOperand(arg1); IsOperand(arg2)]) -> Some (PredicateExpr.BinaryComp("<=", arg1, arg2))
        | SpecificCall <@ (>=) @> (_, _, [IsOperand(arg1); IsOperand(arg2)]) -> Some (PredicateExpr.BinaryComp(">=", arg1, arg2))
        | _ -> None

    let (|IsNegationExpr|_|) (expr: Expr) =
        match expr with
        | SpecificCall <@ not @> (_, _, [arg]) -> Some arg
        | _ -> None

    let rec tryParsePredicate (expr: Expr) =
        match expr with
        | IsBinaryExpr pred -> Some pred
        // NOT (expr)
        | IsNegationExpr expr ->
            tryParsePredicate expr
            |> Option.map (fun pred -> PredicateExpr.Not(pred))
        // It is an AND
        | IfThenElse(ifExpr, thenExpr, Value(:? bool as v, _)) when v = false ->
            tryParsePredicate ifExpr
            |> Option.bind (fun left ->
                tryParsePredicate thenExpr
                |> Option.bind (fun right ->
                    Some <| PredicateExpr.And(left, right)))

        // It is an OR
        | IfThenElse(ifExpr, Value(:? bool as v, _), elseExpr) when v = true ->
            tryParsePredicate ifExpr
            |> Option.bind (fun left ->
                tryParsePredicate elseExpr
                |> Option.bind (fun right ->
                    Some <| PredicateExpr.Or(left, right)))

        | IsUnaryExpr pred -> Some pred
        | _ -> None

[<AutoOpen>]
module QuotationInterpreter =
    type InterpreterResult<'TReturn> = Choice<ICypherFluentQuery, ICypherFluentQuery<'TReturn>>
    
    let private unhandledExpr (expr: Expr) = failwith (sprintf "Unhandled expression: %A" expr)
    let private unsupportedScenario (scenario: string) (expr: Expr) =
        let exnMsg = sprintf "%s (Expr: %A)" scenario expr
        raise <| NotImplementedException(exnMsg)
    let private unexpectedExpr (context: string) (expr: Expr) =
        failwith (sprintf "[Context: %s] Unexpected expression: %A" context expr)

    // Count arguments
    // Generate lambda body
    let private getArgumentsFromTuple (exprList: Expr list) =
        let varMap =
            exprList
            |> List.fold (fun argMap expr ->
                match expr with
                | Var v
                | PropertyGetChain(v, _) ->
                    Map.add v.Name (Var(v.Name, typeof<ICypherResultItem>)) argMap
                | _ -> failwith (sprintf "Unsupported expr: %A" expr)
            ) Map.empty<string, Var>

        let translatedReturnExpr =
            exprList
            |> List.map (fun expr ->
                match expr with
                | Var v ->
                    let asMethod =
                        typeof<ICypherResultItem>.GetMethod("As").MakeGenericMethod(v.Type)
                    Expr.Call(Expr.Var(varMap.[v.Name]), asMethod, [])
                | _ -> failwith (sprintf "Unsupported expr: %A" expr)
            )

        varMap, translatedReturnExpr

    let private makeDelegate<'TFunc> vars body =
        let expression =
            Expr.NewDelegate(typeof<'TFunc>, vars, body)
            |> QuotationToExpression
        let lambda = expression :?> LambdaExpression
        Expression.Lambda<'TFunc>(lambda.Body, lambda.Parameters)

    let private getCypherReturn<'T> (cypher: ICypherFluentQuery) (exprList: Expr list) =
        let varMap, translatedReturnExpr = getArgumentsFromTuple exprList
        
        let body = Expr.NewTuple(translatedReturnExpr)
        let vars = varMap
                   |> Seq.map (fun (KeyValue(_key, value)) -> value)
                   |> Seq.toList

        printfn "Vars: %A" vars
        printfn "Body: %A" body

        match vars with
        | [] ->  makeDelegate<Func<'T>> vars body |> cypher.Return
        | [_] -> makeDelegate<Func<_,'T>> vars body |> cypher.Return
        | [_;_] -> makeDelegate<Func<_,_,'T>> vars body |> cypher.Return
        | [_;_;_] -> makeDelegate<Func<_,_,_,'T>> vars body |> cypher.Return
        | [_;_;_;_] -> makeDelegate<Func<_,_,_,_,'T>> vars body |> cypher.Return
        // TODO denisok: add more cases
        | _ -> failwith "Too much values to return"

    let private executeReturn<'T> (cypher: ICypherFluentQuery) (returnExpr: Expr): InterpreterResult<'T> =
        let cypherExpr = function
            | Var v -> v.Name
            | e -> unexpectedExpr "RETURN statement" e

        match returnExpr with
        | Var v ->
            printfn "Return var: %s (type: %s)" v.Name v.Type.Name
            Choice2Of2 (cypher.Return(cypherExpr returnExpr))

        | NewTuple (exprList) ->
            printfn "Return tuple: %A" exprList
            Choice2Of2 (getCypherReturn cypher exprList)

        | _ -> unhandledExpr returnExpr

    let private executeCreate (cypher: ICypherFluentQuery) createExpr =
        match createExpr with
        | CreateNodeCall nodeExpr -> nodeExpr.Create(cypher)
        | CreateRelationCall patternExpr -> patternExpr.Create(cypher)
        | _ -> unsupportedScenario "Create node" createExpr

    let private executeDelete (cypher: ICypherFluentQuery) deleteExpr =
        match deleteExpr with
        | DeleteNodeCall(Var(node)) ->
            printfn "Delete node: %s (type: %s)" node.Name node.Type.Name
            cypher.Delete(node.Name)
        | DeleteRelationshipCall(Var(rel)) ->
            printfn "Delete relationship: %s (type: %s)" rel.Name rel.Type.Name
            cypher.Delete(rel.Name)
        | _ -> unexpectedExpr "DELETE expression" deleteExpr

    let private executeMerge (cypher: ICypherFluentQuery) mergeExpr =
        match mergeExpr with
        | MergeNodeCall nodeExpr -> nodeExpr.Merge(cypher)
        | MergeRelationCall patternExpr -> patternExpr.Merge(cypher)
        | _ -> unsupportedScenario "MERGE expression" mergeExpr

    let private executeWhere<'T> (cypher: ICypherFluentQuery) (whereExpr: Expr): InterpreterResult<'T> =
        let executeNextExpr context cypher next =
            match next with
            | ReturnResultsCall(returnExpr) -> executeReturn cypher returnExpr
            | CreateNodeCall _
            | CreateRelationCall _ -> Choice1Of2 (executeCreate cypher next)
            | DeleteNodeCall _
            | DeleteRelationshipCall _ -> Choice1Of2 (executeDelete cypher next)
            | MergeRelationCall _ -> Choice1Of2 (executeMerge cypher next)
            | _ -> unexpectedExpr context next
        let context = "After WHERE expression"
            
        match whereExpr with
        | Sequential(expr, next) ->
            let cypher =
                match expr with
                | WhereCall(predOpt) ->
                    match tryParsePredicate predOpt with
                    | Some pred -> 
                        let whereExpr = pred.CypherExpr
                        printfn "Where: %s" whereExpr
                        cypher.Where(whereExpr)
                    | None -> unexpectedExpr "WHERE predicate" expr
                | _ -> cypher

            executeNextExpr context cypher next
        | _ -> executeNextExpr "Not a WHERE expression" cypher whereExpr

    let rec private executeMatch<'T> isFirstCall (cypher: ICypherFluentQuery) (q: Expr): InterpreterResult<'T> =
        let nodeCypherExpr (n: Expr) =
            match n with
            | Var v -> sprintf "(%O:%s)" v.Name v.Type.Name
            | DeclareNodeCall typ -> sprintf "(:%s)" typ.Name
            | _ -> unhandledExpr n

        match q with
        | Sequential(MatchNodeCall(nodeArg), rest) ->
            let matchExpr = nodeCypherExpr nodeArg
            printfn "MatchNode: %A (expr: %s)" nodeArg matchExpr
            executeMatch false (cypher.Match(matchExpr)) rest

        | Sequential(IsMatchRelationClause(matchRel), rest) ->
            let matchExpr = matchRel.CypherExpr
            printfn "MatchRelation: %s"  matchExpr
            executeMatch false (cypher.Match(matchExpr)) rest

        | _ when not isFirstCall -> executeWhere cypher q
        | _ -> unhandledExpr q

    let executeReadQuery (cypher: ICypherFluentQuery) (query: Expr<CypherResult<'T>>): seq<'T> =
        let rec impl cypher (query: Expr) =
            match query with
            | Let(var, expr, rest) ->
                match expr with
                | DeclareNodeCall typ ->
                    printfn "Declare node: %s (type: %s)" var.Name typ.Name
                    impl cypher rest
                | DeclareRelationshipCall typ ->
                    printfn "Declare relationship: %s (type: %s)" var.Name typ.Name
                    impl cypher rest
                | _ -> failwith "Only calls to 'declareNode' or 'declareRelationship' are allowed in a 'let' construct"
            | _ -> executeMatch true cypher query

        match impl cypher query with
        | Choice1Of2 _ -> failwith "Unexpected interpreter result"
        | Choice2Of2(cypherResult) -> cypherResult.Results

    let executeWriteQuery (cypher: ICypherFluentQuery) (query: Expr<unit>): unit =
        let rec impl cypher (query: Expr) =
            match query with
            // Create node
            | CreateNodeCall(_) as createExpr -> Choice1Of2 (executeCreate cypher createExpr)
            // TODO denisok: this is not good: useful data should be extracted -> the ActivePatterns are executed many times
            | MergeNodeCall(_) as mergeExpr -> Choice1Of2 (executeMerge cypher mergeExpr)
            | Sequential(CreateNodeCall(_) as createExpr, rest) ->
                let newCypher = executeCreate cypher createExpr
                impl newCypher rest

            // Declare node/relationship
            | Let(var, expr, rest) ->
                match expr with
                | DeclareNodeCall typ ->
                    printfn "Declare node: %s (type: %s)" var.Name typ.Name
                    impl cypher rest
                | DeclareRelationshipCall typ ->
                    printfn "Declare relationship: %s (type: %s)" var.Name typ.Name
                    impl cypher rest
                | _ -> failwith "Only calls to 'declareNode' or 'declareRelationship' are allowed in a 'let' construct"

            // Could be a match query
            | _ -> executeMatch true cypher query

        match impl cypher query with
        | Choice1Of2 cypher -> cypher.ExecuteWithoutResults()
        | Choice2Of2 _ -> failwith "Unexpected interpreter result"
