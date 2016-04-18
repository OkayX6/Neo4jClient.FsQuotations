module Neo4jClient.FsQuotations.Tests

open System
open NUnit.Framework
open Neo4jClient
open Neo4jClient.FsQuotations

let neo4jClient =
    let client = new GraphClient(Uri("http://localhost:7474/db/data"), "neo4j", "Password123")
    client.Connect()
    client

// TODO denisok: this shouldn't be executed for every tests (read queries)
[<SetUp>]
let setupDbWithTestData () = initDbWithTestData neo4jClient

[<Test>]
let ``Get all nodes with specific label`` () =
    let query =
        <@
        let u = declareNode<UserNode>
        matchNode u
        returnResults u
        @>

    let results =
        query
        |> executeQuery neo4jClient.Cypher
        |> Seq.map (fun user -> user.FacebookId)
        |> Set.ofSeq

    Assert.AreEqual(
        set [ "Denis"; "TT"; "Opwal"; "Chouchou" ],
        results,
        "Get all user nodes")

[<Test>]
let ``Get nodes with basic WHERE clause`` () =
    let query =
        <@
        let u = declareNode<UserNode>
        matchNode u
        where (u.FacebookId = "Denis")
        returnResults u
        @>

    let results =
        query
        |> executeQuery neo4jClient.Cypher
        |> Seq.map (fun user -> user.FacebookId)
        |> Seq.toArray

    Assert.AreEqual(1, results.Length, "Number of results")
    Assert.AreEqual("Denis", results.[0], "Facebook ID")

[<Test>]
let ``Get pairs of nodes having specific relationship`` () =
    let query =
        <@
        let user = declareNode<UserNode>
        let household = declareNode<HouseholdNode>
        matchRelation user declareRelationship<IsResidentOf> household
        returnResults (user, household)
        @>

    let results =
        query
        |> executeQuery neo4jClient.Cypher
        |> Seq.toArray

    Assert.AreEqual(3, results.Length, "Number of results")

[<Test>]
let ``Get all nodes having relationship with another`` () =
    let query =
        <@
        let user = declareNode<UserNode>
        let household = declareNode<HouseholdNode>
        matchRelation user declareRelationship<IsResidentOf> household
        where (household.Name = "Coloc de la Joie")
        returnResults user
        @>

    let results =
        query
        |> executeQuery neo4jClient.Cypher
        |> Set.ofSeq
        |> Set.map (fun user -> user.FacebookId)

    Assert.AreEqual(set [ "Denis"; "TT"; "Opwal" ], results, "Residents of the 'Coloc de la Joie' household")

[<Test>]
let ``Match any relationships of a specific type`` () =
    let query =
        <@
        let r = declareRelationship<IsResidentOf>
        matchRelation declareNode<UserNode> r declareNode<HouseholdNode>
        returnResults r
        @>

    let results =
        query
        |> executeQuery neo4jClient.Cypher
        |> Seq.toArray

    Assert.AreEqual(3, results.Length, "Number of residency relationships")

[<Test>]
let ``Match on right relationships`` () =
    let query =
        <@
        let r = declareRelationship<IsResidentOf>
        matchRightRelation declareNode<UserNode> r declareNode<HouseholdNode>
        returnResults r
        @>

    let results =
        query
        |> executeQuery neo4jClient.Cypher
        |> Seq.toArray

    Assert.AreEqual(3, results.Length, "Number of residency right-relationships")

[<Test>]
let ``Match on left relationships`` () =
    let query =
        <@
        let r = declareRelationship<IsResidentOf>
        matchLeftRelation declareNode<UserNode> r declareNode<HouseholdNode>
        returnResults r
        @>

    let results =
        query
        |> executeQuery neo4jClient.Cypher
        |> Seq.toArray

    Assert.AreEqual(0, results.Length, "Number of residency left-relationships")