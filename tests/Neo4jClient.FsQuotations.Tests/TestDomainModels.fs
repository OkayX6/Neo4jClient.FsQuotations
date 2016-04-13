[<AutoOpen>]
module Neo4jClient.FsQuotations.TestDomainModels

open Neo4jClient.FsQuotations
open Neo4jClient

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

let initDbWithTestData (client: GraphClient) =
    // prepare data for tests
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