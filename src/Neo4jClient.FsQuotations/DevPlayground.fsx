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
open System.Linq.Expressions
open Neo4jClient.Cypher

<@
let n = declareNode<UserNode>
matchNode n
returnResults n
@>
|> executeReadQuery client.Cypher
|> Seq.toArray

do
    let newHousehold: HouseholdNode = { Name = "Maison à Bussy" }
    <@
    let invitee = declareNode<UserNode>
    matchNode invitee
    where (invitee.FacebookId = "12345")
    createUniqueRightRelation invitee declareRelationship<IsResidentOf> newHousehold
    @>
    |> executeWriteQuery client.Cypher

<@
let invitee = declareNode<UserNode>
let hh = declareNode<HouseholdNode>
matchRightRelation invitee declareRelationship<IsResidentOf> hh
where (invitee.FacebookId = "12345")
returnResults (invitee, hh)
@>
|> executeReadQuery client.Cypher
