module Neo4jClient.FsQuotations.Tests

open System
open NUnit.Framework
open Neo4jClient
open Neo4jClient.FsQuotations

let neo4jClient =
    let client = new GraphClient(Uri("http://localhost:7474/db/data"), "neo4j", "Password123")
    client.Connect()
    client

[<AutoOpen>]
module TestDomainModels =
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

[<Test>]
let ``Get all nodes with specific label`` () =
    // Read queries
    let query =
        <@
        let u = declareNode<UserNode>()
        matchNode u
        returnResults u
        @>

    let results =
        query
        |> executeReadQuery<UserNode> neo4jClient.Cypher
        |> Seq.toArray

    Assert.AreEqual(4, results.Length, "Number of results")
