#r @"..\..\packages\Neo4jClient\lib\net45\Neo4jClient.dll"

#load "StringUtils.fs"
      "Cypher.fs"
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
    
    let clearAllRelations () =
        client.Cypher.Match("()-[r]->()").Delete("r").ExecuteWithoutResults()
    let clearAllNodes () =
        client.Cypher.Match("(n)").Delete("n").ExecuteWithoutResults()

    clearAllRelations ()
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


open FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Quotations.Patterns
open FSharp.Linq.RuntimeHelpers
open Neo4jClient.Cypher
open System.Linq.Expressions


client.Cypher
    .Match("(u: UserNode), (v: UserNode)")
    .Where("u.FacebookId = '12345' AND v.FacebookId = '45678'")
    .Return(
        <@ Func<_,_,_>(fun (u: ICypherResultItem) (v: ICypherResultItem) -> u.As<UserNode>(), v.As<UserNode>()) @>
        |> QuotationToLambdaExpression
    )
    .Results

let queryRelation =
    <@
    let user = declareNode<UserNode>
    let household = declareNode<HouseholdNode>
    matchRelation user declareRelationship<IsResidentOf> household
    returnResults (user, household)
    @>

let res =
    queryRelation
    |> executeReadQuery client.Cypher 
    |> Seq.toArray


//let readQuery =
//    <@
//    let u = declareNode<UserNode>()
//    matchRelation u (declareRelationship<IsResidentOf>()) (declareNode<HouseholdNode>())
//    where (u.FacebookId = "12345")
//    returnResults u
//    @>
