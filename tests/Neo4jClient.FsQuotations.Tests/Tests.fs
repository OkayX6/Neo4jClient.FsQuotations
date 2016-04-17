module Neo4jClient.FsQuotations.Tests

open System
open NUnit.Framework
open Neo4jClient
open Neo4jClient.FsQuotations

let neo4jClient =
    let client = new GraphClient(Uri("http://localhost:7474/db/data"), "neo4j", "Password123")
    client.Connect()
    client

[<SetUp>]
let setupDbWithTestData () = initDbWithTestData neo4jClient

[<Test>]
let ``Get all nodes with specific label`` () =
    let query =
        <@
        let u = declareNode<UserNode>()
        matchNode u
        returnResults u
        @>

    let results =
        query
        |> executeReadQuery<UserNode> neo4jClient.Cypher
        |> Seq.map (fun user -> user.FacebookId)
        |> Set.ofSeq

    Assert.AreEqual(
        set [ "Denis"; "TT"; "Opwal"; "Chouchou" ],
        results,
        "Get all user nodes")

[<Test>]
let ``MATCH-WHERE-RETURN with equality`` () =
    let query =
        <@
        let u = declareNode<UserNode>()
        matchNode u
        where (u.FacebookId = "Denis")
        returnResults u
        @>

    let results =
        query
        |> executeReadQuery<UserNode> neo4jClient.Cypher
        |> Seq.map (fun user -> user.FacebookId)
        |> Seq.toArray

    Assert.AreEqual(1, results.Length, "Number of results")
    Assert.AreEqual("Denis", results.[0], "Facebook ID")
