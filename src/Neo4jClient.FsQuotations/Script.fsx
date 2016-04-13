#r @"..\..\packages\Neo4jClient\lib\net45\Neo4jClient.dll"

#load "Cypher.fs"
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

//    let inline mergeNode (node: 'T when 'T :> ICypherNode) =
//        let nodeTypeName = typeof<'T>.Name
//        client
//            .Cypher
//            .Merge(sprintf "(node:%s {nodeParam})" nodeTypeName)
//            .WithParam("nodeParam", node)
//            .ExecuteWithoutResults()
    
    let clearAllNodes () =
        client.Cypher.Match("(n)").Delete("n").ExecuteWithoutResults()

    clearAllNodes ()

    createNode userDenis
    createNode userTT
    createNode userOpwal
    createNode userChou2

    createNode householdColocJoie
    createNode householdColoCarrouf

// Write query

// Read queries
let getAllUserNodesQuery =
    <@
    let u = declareNode<UserNode>()
    matchNode u
    returnResults u
    @>

let res =
    executeReadQuery<UserNode> client.Cypher getAllUserNodesQuery
    |> Seq.toArray

//let readQuery =
//    <@
//    let u = declareNode<UserNode>()
//    matchRelation u (declareRelationship<IsResidentOf>()) (declareNode<HouseholdNode>())
//    where (u.FacebookId = "12345")
//    returnResults u
//    @>
