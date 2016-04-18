[<AutoOpen>]
module Neo4jClient.FsQuotations.Tests.TestContext

open System
open Neo4jClient

let neo4jClient =
    let client = new GraphClient(Uri("http://localhost:7474/db/data"), "neo4j", "Password123")
    client.Connect()
    client