#r @"..\..\packages\Neo4jClient\lib\net45\Neo4jClient.dll"

#load "Cypher.fs"
      "Interpreter.fs"

open System
open Neo4jClient
open Neo4jClient.FsQuotations

[<CLIMutable>]
type UserNode = 
    { FacebookId: string }
    interface ICypherNode

[<CLIMutable>]
type IsResidentOf =
    { CustomHouseholdName: string }
    interface ICypherRelationship

[<CLIMutable>]
type HouseholdNode = 
    { Name: string }
    interface ICypherNode


let client = new GraphClient(new Uri("http://localhost:7474/db/data"), "neo4j", "Password123");
do
    client.Connect()

// prepare data for tests
do
    let userDenis = { FacebookId = "12345" }
    let userTT = { FacebookId = "45678" }
    let userOpwal = { FacebookId = "67890" }
    let userChou2 = { FacebookId = "FbChouchou" }

    let householdColocJoie = { Name = "Coloc de la Joie" }
    let householdColoCarrouf = { Name = "Colocarouf" }

    let createNode (node: 'T when 'T :> ICypherNode) =
        let nodeTypeName = typeof<'T>.Name
        client
            .Cypher
            .Create(sprintf "(node:%s {nodeParam})" nodeTypeName)
            .WithParam("nodeParam", node)
            .ExecuteWithoutResults()

//    let inline mergeNode (node: 'T when 'T :> ICypherNode) =
//        let nodeTypeName = typeof<'T>.Name
//        client
//            .Cypher
//            .Merge(sprintf "(node:%s {nodeParam})" nodeTypeName)
//            .WithParam("nodeParam", node)
//            .ExecuteWithoutResults()
    
    let clearAllNodes () =
        client.Cypher.Match("(n)").Delete("n").ExecuteWithoutResults()

    clearAllNodes ()

    createNode userDenis
    createNode userTT
    createNode userOpwal
    createNode userChou2

    createNode householdColocJoie
    createNode householdColoCarrouf

// Write query

// Read queries
let getAllUserNodesQuery =
    <@
    let u = declareNode<UserNode>()
    matchNode u
    returnResults u
    @>

open System.Reflection
open System.Collections.Generic
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers
open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Neo4jClient.Cypher

let fsQuotationAssembly = matchNode.GetType().DeclaringType.Assembly

//type CypherReadQueryStateChecker =
//    {
//        NodeMatches: Dictionary<string, string>
//    }
//
//    member x.AddNode(name: string, typeName: string) =
//        match x.NodeMatches.TryGetValue(name) with

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

let res =
    executeReadQuery<UserNode> client.Cypher getAllUserNodesQuery
    |> Seq.toArray

//let readQuery =
//    <@
//    let u = declareNode<UserNode>()
//    matchRelation u (declareRelationship<IsResidentOf>()) (declareNode<HouseholdNode>())
//    where (u.FacebookId = "12345")
//    returnResults u
//    @>
