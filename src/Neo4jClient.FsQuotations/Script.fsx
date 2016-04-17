#r @"..\..\packages\Neo4jClient\lib\net45\Neo4jClient.dll"

#load "Cypher.fs"
      "Interpreter.fs"

open System
open Neo4jClient
open Neo4jClient.FsQuotations

[<CLIMutable>]
type UserNode = 
    { FacebookId: string }
    interface INeo4jNode

[<CLIMutable>]
type IsResidentOf =
    { CustomHouseholdName: string }
    interface INeo4jRelationship

[<CLIMutable>]
type HouseholdNode = 
    { Name: string }
    interface INeo4jNode


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

    let createNode (node: 'T when 'T :> INeo4jNode) =
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

//let rec substituteExpr expression =
//    match expression with
//    | SpecificCall <@@ (=) @@> (_, _, exprList) ->
//        let lhs = substituteExpr exprList.Head
//        let rhs = substituteExpr exprList.Tail.Head
//        <@@ mul %%lhs %%rhs @@>
//    | ShapeVar var -> Expr.Var var
//    | ShapeLambda (var, expr) -> Expr.Lambda (var, substituteExpr expr)
//    | ShapeCombination(shapeComboObject, exprList) ->
//        RebuildShapeCombination(shapeComboObject, List.map substituteExpr exprList)

// Read queries
let getSpecificNodeQuery =
    <@
    let u = declareNode<UserNode>
    matchNode u
    where (u.FacebookId = "12345")
    returnResults u
    @>

open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.Patterns
open Neo4jClient.Cypher

let (Let(_, _, getExpr)) =
    <@
        let u = declareNode<UserNode>
        where (u.FacebookId = "123")
    @>

client.Cypher
    .Match("(u: UserNode)")
    .Where("u.FacebookId = \"12345\"")
    .Return(
        <@
        Func<_, _>(fun (u: ICypherResultItem) -> u.As<UserNode>(), u.Count())
        @>
        |> QuotationToLambdaExpression
    )
    .Results

let res =
    getSpecificNodeQuery
    |> executeReadQuery client.Cypher 
    |> Seq.toArray

//let readQuery =
//    <@
//    let u = declareNode<UserNode>()
//    matchRelation u (declareRelationship<IsResidentOf>()) (declareNode<HouseholdNode>())
//    where (u.FacebookId = "12345")
//    returnResults u
//    @>
