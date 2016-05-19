#load "TestData.fsx"

open System
open Neo4jClient
open Neo4jClient.FsQuotations
open TestData

#time "on"

// Create client
let client = new GraphClient(new Uri("http://localhost:7474/db/data"), "neo4j", "Password123");
do client.Connect()

// Init test data
do initTestData client

open FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Quotations.Patterns
open FSharp.Linq.RuntimeHelpers
open System.Linq.Expressions
open Neo4jClient.Cypher


let createNodeAndExecute (node: #INeo4jNode) =
    <@ createNode node @>
    |> executeWriteQuery client.Cypher

createNodeAndExecute { FacebookId = "Parisian" }

let cuir = { FacebookId = "Cuir" }
type MyNode =
    {
        Prop1: string
    }
    interface INeo4jNode

let node = { Prop1 = "abc" }
<@
mergeNode node
@>
|> executeWriteQuery client.Cypher

// Act
// TODO denisok: bug when value is propertyget
do
    let newHousehold: HouseholdNode = { Name = "Paris 10" }
    <@
        let parisian = declareNode<UserNode>
        matchNode parisian
        where (parisian.FacebookId = "Parisian")
        createRightRelation parisian declareRelationship<IsResidentOf> newHousehold
    @>
    |> executeWriteQuery client.Cypher

<@ let u = declareNode<UserNode>
   matchNode u
   returnResults u @>
|> executeReadQuery client.Cypher
|> Seq.toList

let query =
    <@
    let r = declareRelationship<IsResidentOf>
    matchLeftRelation declareNode<UserNode> r declareNode<HouseholdNode>
    returnResults r
    @>

let (Let(_, _, Sequential(m, _))) = query

open QuotationsHelpers

match m with
| IsMatchRelationClause(_) -> true
| _ -> false


let results =
    query
    |> executeReadQuery client.Cypher
    |> Seq.toArray


//let relationship = {  }
let denisRelationToColocDeLaJoie =
    <@
    let user = declareNode<UserNode>
    let house = declareNode<HouseholdNode>
    let rel = declareRelationship<IsResidentOf>
    matchRelation user rel house
    where (user.FacebookId = "Denis" && house.Name = "Coloc de la Joie")
    returnResults (user, rel, house)
    @>
    |> executeReadQuery client.Cypher
    |> Seq.toList

do
    let rel: IsResidentOf = { CustomHouseholdName = "Maison à République" }
    <@
    let user = declareNode<UserNode>
    let house = declareNode<HouseholdNode>
    matchNode user
    matchNode house
    where (user.FacebookId = "Denis" && house.Name = "Coloc de la Joie")
    mergeRightRelation user rel house
    @>
    |> executeWriteQuery client.Cypher

// TODO denisok: shouldn't allow relationship creation if no default constructor - except if nullable
client
    .Cypher
    .Match("(user: UserNode)")
    .Match("(house: HouseholdNode)")
    .Where(fun (user: UserNode) -> user.FacebookId = "Denis")
    .AndWhere(fun (house: HouseholdNode) -> house.Name = "Coloc de la Joie")
    .Merge("(user)-[r:IsResidentOf]->(house)")
    .OnCreate()
    .Set("r = {myParam}")
    .WithParam("myParam", { CustomHouseholdName = "Maison à République" })
    .ExecuteWithoutResults()
