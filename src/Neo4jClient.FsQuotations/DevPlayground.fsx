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

//<@
//where ((1 = 2 || 1 = 3) && 1 = 3)
//where (1 = 2 || 1 = 2 || 1 = 3)
//where (1 = 2 && 1 = 2 && 1 = 3)
//where (1 = 2 && 1 = 3)
//where (1 = 2)
//@>

<@
let u = declareNode<UserNode>
matchNode u
where (u.FacebookId > "12344" && u.FacebookId >= "12346")
returnResults u
@>
|> executeReadQuery client.Cypher

//<@
//let isResidentRel = declareRelationship<IsResidentOf>
//matchRightRelation declareNode<UserNode> isResidentRel declareNode<HouseholdNode>
//returnResults isResidentRel
//@>
//|> executeReadQuery client.Cypher
//
//<@
//let isResidentRel = declareRelationship<IsResidentOf>
//matchRightRelation declareNode<UserNode> isResidentRel declareNode<HouseholdNode>
//deleteRelationship isResidentRel
//@>
//|> executeWriteQuery client.Cypher
