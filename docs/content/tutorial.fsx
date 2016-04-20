(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "./../../bin/Neo4jClient.FsQuotations"

(**
Introducing your project
========================

Reference this library (dependency on Neo4jClient) and create a Neo4j GraphClient().

*)
#r "Neo4jClient.dll"
#r "Neo4jClient.FsQuotations.dll"

open System
open Neo4jClient
open Neo4jClient.FsQuotations

let client = new GraphClient(Uri("http://localhost:7474/db/data"), "neo4j", "Password123")
client.Connect()

(**
Define nodes & relationships in your model
*)

[<CLIMutable>]
type UserNode = 
    { FacebookId: string }
    interface INeo4jNode

(**
Create a node
*)

let node = { FacebookId = "Zuckerberg" }
<@ createNode node @>
|> executeWriteQuery client.Cypher

(**
Query all nodes of a specific type
*)

let results = 
    <@ let n = declareNode<UserNode>
       matchNode n
       returnResults n @>
    |> executeReadQuery client.Cypher