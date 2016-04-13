namespace Neo4jClient.FsQuotations

open System
open System.Reflection
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers
open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
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
    let declareNode<'T when 'T :> INeo4jNode> () = Unchecked.defaultof<'T>

    let declareRelationship<'T when 'T :> INeo4jRelationship> () = Unchecked.defaultof<'T>

    /// Matches a node with a specific type like: "MATCH (n: 'T)"
    ///
    /// ## Type parameters
    ///  - `'T` - the type of the node
    let matchNode (_: #INeo4jNode) = ()

    let matchRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let optionalMatchNode (_: #INeo4jNode) = ()
    let optionalMatchRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let where (_boolExpr: bool) = ()
    let returnResults (_: 'T): 'T = Unchecked.defaultof<'T>

[<AutoOpen>]
module internal QuotationsHelpers =
    let fsQuotationAssembly = matchNode.GetType().DeclaringType.Assembly

    let inline (|IsPredefinedMethod|_|) name (methodInfo: MethodInfo) =
        if methodInfo.Name = name &&
           methodInfo.DeclaringType.Assembly = fsQuotationAssembly
        then Some ()
        else None

    let inline (|DeclareNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | Call(None, IsPredefinedMethod "declareNode", []) -> Some ()
        | _ -> None

    let inline (|MatchNodeCall|_|) (callExpr: Expr) =
        match callExpr with
        | Call(None, IsPredefinedMethod "matchNode", [nodeArg]) -> Some nodeArg
        | _ -> None

    let inline (|MatchRelationCall|_|) (callExpr: Expr) =
        match callExpr with
        | Call(None, IsPredefinedMethod "matchRelation", [nodeArg1 ; relArg ; nodeArg2])
            -> Some (nodeArg1, relArg, nodeArg2)
        | _ -> None

    let inline (|ReturnResultsCall|_|) (callExpr: Expr) =
        match callExpr with
        | Call(None, IsPredefinedMethod "returnResults", [returnArg]) -> Some returnArg
        | _ -> None

[<AutoOpen>]
module QuotationInterpreter =
    let unhandledExpr (expr: Expr) = failwith (sprintf "Unhandled expression: %A" expr)

    let executeReturn<'T> (cypher: ICypherFluentQuery) (returnExpr: Expr): seq<'T> =
        match returnExpr with
        | Var v ->
            printfn "Return var: %s (type: %s)" v.Name v.Type.Name

            cypher.Return(sprintf "%s" v.Name).Results

        | _ -> unhandledExpr returnExpr

    let rec executeMatch<'T> (cypher: ICypherFluentQuery) (q: Expr): seq<'T> =
        let executeRest cypher (rest: Expr): seq<'T> =
            match rest with
            | Sequential((MatchNodeCall(_) | MatchRelationCall(_)), _) -> executeMatch cypher rest
            | ReturnResultsCall returnArg -> executeReturn cypher returnArg
            | _ -> unhandledExpr rest

        match q with
        | Sequential(MatchNodeCall(nodeArg), rest) ->
            let matchExpr = sprintf "(%O: %s)" nodeArg nodeArg.Type.Name
            printfn "MatchNode: %A (expr: %s)" nodeArg matchExpr
            executeRest (cypher.Match(matchExpr)) rest

        | Sequential(MatchRelationCall(nodeArg), rest) ->
            printfn "MatchRelation: %A" nodeArg
            executeRest cypher rest

        | _ -> unhandledExpr q

    let rec executeReadQuery<'T> (cypher: ICypherFluentQuery) (query: Expr): seq<'T> =
        match query with
        | Let(var, expr, rest) when var.Name.Length > 0 ->
            match expr with
            | DeclareNodeCall ->
                printfn "Declare node: %s (type: %s)" var.Name var.Type.Name
                match rest with
                | Let(_) -> executeReadQuery<'T> cypher rest
                | _ -> executeMatch cypher rest
            | Patterns.Call(None, methodInfo, []) when methodInfo.Name = "declareNode" ->
                printfn "Declare relationship: %s (type: %s)" var.Name var.Type.Name
                match rest with
                | Let(_) -> executeReadQuery cypher rest
                | _ -> executeMatch cypher rest

            | _ -> failwith "Only calls to 'declareNode' or 'declareRelationship' are allowed in a 'let' construct"
        | Let(_) -> failwith "Invalid 'let' expression: bounded value must have a name"
        | _ -> unhandledExpr query


/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h

//type CypherReadQueryStateChecker =
//    {
//        NodeMatches: Dictionary<string, string>
//    }
//
//    member x.AddNode(name: string, typeName: string) =
//        match x.NodeMatches.TryGetValue(name) with