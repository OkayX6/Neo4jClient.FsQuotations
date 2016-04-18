#load "TestData.fsx"

open System
open Neo4jClient
open Neo4jClient.FsQuotations
open TestData

// Create client
let client = new GraphClient(new Uri("http://localhost:7474/db/data"), "neo4j", "Password123");
do client.Connect()

// Init test data
do initTestData client

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


open FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Quotations.Patterns
open FSharp.Linq.RuntimeHelpers
open Neo4jClient.Cypher
open System.Linq.Expressions

let getNodes() =
    <@
    let n = declareNode<UserNode>
    matchNode n
    where (n.FacebookId = "newNode")
    returnResults n
    @>
    |> executeReadQuery client.Cypher
    |> Seq.toArray

do
    let u: UserNode = { FacebookId = "newNode" }

    <@
    createNode u
    @>
    |> executeWriteQuery client.Cypher

getNodes() // Before deletion

do
    <@
    let u = declareNode<UserNode>
    matchNode u
    where (u.FacebookId = "newNode")
    deleteNode u
    @>
    |> executeWriteQuery client.Cypher

getNodes() // After deletion

client.Cypher
    .Match("(n: UserNode)")
    .Where("n.FacebookId = 'newNode'")
    .Delete("n")
    .ExecuteWithoutResults

