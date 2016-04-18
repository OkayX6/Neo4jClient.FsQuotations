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

type INeo4jNode = interface end
type INeo4jRelationship = interface end

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///

[<AutoOpen>]
module CypherQueryGrammar = 
    /// Declares a node with a specific type
    ///
    /// ## Type parameters
    ///  - `'T` - the type of the node
    let declareNode<'T when 'T :> INeo4jNode> = Unchecked.defaultof<'T>
    let declareRelationship<'T when 'T :> INeo4jRelationship> = Unchecked.defaultof<'T>

    /// Matches a node with a specific type like: "MATCH (n: 'T)"
    ///
    /// ## Type parameters
    ///  - `'T` - the type of the node
    let matchNode (_: #INeo4jNode) = ()

    let matchRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let matchLeftRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let matchRightRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let optionalMatchNode (_: #INeo4jNode) = ()
    let optionalMatchRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let where (_boolExpr: bool) = ()

    let createNode (_: 'TNode when 'TNode :> INeo4jNode) = ()
    let deleteNode (_: 'TNode when 'TNode :> INeo4jNode) = ()

    let returnResults (_: 'T): 'T = Unchecked.defaultof<'T>

[<AutoOpen>]
module internal QuotationsHelpers =
    let fsQuotationAssembly = matchNode.GetType().DeclaringType.Assembly

    let inline (|IsPredefinedMethod|_|) name (methodInfo: MethodInfo) =
        if methodInfo.Name = name &&
           methodInfo.DeclaringType.Assembly = fsQuotationAssembly
        then Some ()
        else None

    let inline (|CreateNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ createNode @> (_, _, [node]) -> Some node
        | _ -> None

    let inline (|DeleteNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ deleteNode @> (_, _, [node]) -> Some node
        | _ -> None

    let inline (|DeclareNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ declareNode @> (_, [nodeType], _) -> Some nodeType
        | _ -> None

    let inline (|DeclareRelationshipCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ declareRelationship @> (_, [relType], _) -> Some relType
        | _ -> None

    let inline (|MatchNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ matchNode @> (_, _, [node]) -> Some node
        | _ -> None

    let inline (|WhereCall|_|) (callExpr: Expr) =
        match callExpr with
        | SpecificCall <@ where @> (_, _, [arg]) -> Some arg
        | _ -> None

    let inline (|AnyMatchRelation|_|) (callExpr: Expr) =
        match callExpr with
        | Call(None,
               ( IsPredefinedMethod "matchRelation"
               | IsPredefinedMethod "matchRightRelation"
               | IsPredefinedMethod "matchLeftRelation" ),
               [nodeArg1 ; relArg ; nodeArg2])
            -> Some (nodeArg1, relArg, nodeArg2)
        | _ -> None

    let inline (|ReturnResultsCall|_|) (callExpr: Expr) =
        match callExpr with
        | Call(None, IsPredefinedMethod "returnResults", [returnArg]) -> Some returnArg
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

    let (|BinaryExpr|_|) (expr: Expr) =
        match expr with
        // NOTE: refactoring the pattern matches lead to exponential type inference/compile time
        | SpecificCall <@ (=) @> (_, _, [arg1; arg2]) -> Some ("=", arg1, arg2)
        | SpecificCall <@ (<>) @> (_, _, [arg1; arg2]) -> Some ("<>", arg1, arg2)
        | SpecificCall <@ (<) @> (_, _, [arg1; arg2]) -> Some ("<", arg1, arg2)
        | SpecificCall <@ (>) @> (_, _, [arg1; arg2]) -> Some (">", arg1, arg2)
        | SpecificCall <@ (<=) @> (_, _, [arg1; arg2]) -> Some ("<=", arg1, arg2)
        | SpecificCall <@ (>=) @> (_, _, [arg1; arg2]) -> Some (">=", arg1, arg2)
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

    let private getOperand (expr: Expr) =
        match expr with
        | PropertyGetChain(entity, propChain) ->
            [|
                yield entity.Name
                yield! propChain
            |]
            |> fun strings -> String.Join(".", strings)
        | Value(value, _typ) ->
            match value with
            | :? string -> sprintf "\"%O\"" value
            // TODO denisok: more types to support
            | :? int | :? bool
            | :? float -> string value
            | _ -> unexpectedExpr "Get operand value" expr
        | _ -> unexpectedExpr "Get operand" expr

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
        | CreateNodeCall(ValueWithName(value, typ, name)) ->
             printfn "Create node: %s (type: %s - value: %A)" name typ.Name value
             // TODO denisok: extract constant
             let paramName = sprintf "__neo4jfsquot__%s" name
             let createExpr = sprintf "(%s:%s {%s})" name typ.Name paramName
             cypher.Create(createExpr)
                   .WithParam(paramName, value)

        | _ -> unsupportedScenario "Create node" createExpr

    let private executeDelete (cypher: ICypherFluentQuery) deleteExpr =
        match deleteExpr with
        | DeleteNodeCall(Var(node)) ->
             printfn "Delete node: %s (type: %s)" node.Name node.Type.Name
             cypher.Delete(node.Name)
        | _ -> unsupportedScenario "Delete node" deleteExpr

    let private executeWhere<'T> (cypher: ICypherFluentQuery) (whereExpr: Expr): InterpreterResult<'T> =
        match whereExpr with
        | Sequential(WhereCall(BinaryExpr(op, arg1, arg2)), rest) ->
            let formattedArg1 = getOperand arg1
            let formattedArg2 = getOperand arg2
            let whereExpr = sprintf "%s %s %s" formattedArg1 op formattedArg2
            printfn "Where %s" whereExpr
            let cypher = cypher.Where(whereExpr)

            match rest with
            | ReturnResultsCall(returnExpr) -> executeReturn cypher returnExpr
            | CreateNodeCall _ -> Choice1Of2 (executeCreate cypher rest)
            | DeleteNodeCall _ -> Choice1Of2 (executeDelete cypher rest)
            | _ -> unexpectedExpr "After 'where' expression" rest
        | _ -> unhandledExpr whereExpr

    let rec private executeMatch<'T> (cypher: ICypherFluentQuery) (q: Expr): InterpreterResult<'T> =
        let executeRest cypher (rest: Expr) =
            match rest with
            | Sequential((MatchNodeCall(_) | AnyMatchRelation(_)), _) -> executeMatch cypher rest
            | Sequential(WhereCall _, _) -> executeWhere cypher rest
            // TODO denisok: find a way to refactor execution branching with executeWhere
            | ReturnResultsCall returnArg -> executeReturn cypher returnArg
            | _ -> unhandledExpr rest

        let nodeCypherExpr (n: Expr) =
            match n with
            | Var v -> sprintf "(%O:%s)" v.Name v.Type.Name
            | DeclareNodeCall typ -> sprintf "(:%s)" typ.Name
            | _ -> unhandledExpr n

        match q with
        | Sequential(MatchNodeCall(nodeArg), rest) ->
            let matchExpr = nodeCypherExpr nodeArg
            printfn "MatchNode: %A (expr: %s)" nodeArg matchExpr
            executeRest (cypher.Match(matchExpr)) rest

        | Sequential(AnyMatchRelation(node1, rel, node2) as matchRel, rest) ->
            let relCypherExpr (r: Expr) =
                match r with
                | Var v -> sprintf "[%s:%s]" v.Name v.Type.Name
                | DeclareRelationshipCall typ -> sprintf "[:%s]" typ.Name
                | _ -> unhandledExpr r

            let matchExpr =
                let formatter =
                    match matchRel with
                    | SpecificCall <@ matchRelation @> _ -> sprintf "%s-%s-%s"
                    | SpecificCall <@ matchRightRelation @> _ -> sprintf "%s-%s->%s"
                    | SpecificCall <@ matchLeftRelation @> _ -> sprintf "%s<-%s-%s"
                    | _ -> unhandledExpr matchRel

                formatter (nodeCypherExpr node1) (relCypherExpr rel) (nodeCypherExpr node2)

            printfn "MatchRelation: %s" matchExpr
            let newCypher = cypher.Match(matchExpr)
            executeRest newCypher rest

        | _ -> unhandledExpr q

    let executeReadQuery (cypher: ICypherFluentQuery) (query: Expr<'T>): seq<'T> =
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
            | _ -> executeMatch cypher query

        match impl cypher query with
        | Choice1Of2 _ -> failwith "Unexpected interpreter result"
        | Choice2Of2(cypherResult) -> cypherResult.Results

    let executeWriteQuery (cypher: ICypherFluentQuery) (query: Expr<unit>): unit =
        let rec impl cypher (query: Expr) =
            match query with
            // Create node
            | CreateNodeCall(_) as createExpr -> Choice1Of2 (executeCreate cypher createExpr)
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
            | _ -> executeMatch cypher query

        match impl cypher query with
        | Choice1Of2 cypher -> cypher.ExecuteWithoutResults()
        | Choice2Of2 _ -> failwith "Unexpected interpreter result"
