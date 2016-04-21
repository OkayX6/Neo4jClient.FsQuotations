module Neo4jClient.FsQuotations.Tests.WriteQueryTests

open System
open NUnit.Framework
open Neo4jClient
open Neo4jClient.FsQuotations

[<SetUp>]
let setupDbWithTestData () =
    clearAllRelations neo4jClient
    clearAllNodes neo4jClient

[<Test>]
let ``Create node`` () =
    for _ in 1 .. 3 do
        let u = { FacebookId = "newNode" }
        <@
        createNode u
        @>
        |> executeWriteQuery neo4jClient.Cypher

    let res =
        <@
        let u = declareNode<UserNode>
        matchNode u
        where (u.FacebookId = "newNode")
        returnResults u
        @>
        |> executeReadQuery neo4jClient.Cypher
        |> Seq.toArray

    Assert.AreEqual(3, res.Length, "Nodes created")

[<Test>]
let ``Delete node`` () =
    let getUserNodes () =
        <@ let u = declareNode<UserNode>
           matchNode u
           where (u.FacebookId = "nodeToDelete")
           returnResults u @>
        |> executeReadQuery neo4jClient.Cypher
        |> Seq.toArray

    createNodeAndExecute neo4jClient { FacebookId = "nodeToDelete" }

    Assert.AreEqual(1, getUserNodes().Length, "Nodes before deletion")

    <@
        let u = declareNode<UserNode>
        matchNode u
        where (u.FacebookId = "nodeToDelete")
        deleteNode u
    @>
    |> executeWriteQuery neo4jClient.Cypher

    Assert.AreEqual(0, getUserNodes().Length, "Nodes after deletion")

[<Test>]
let ``Create and node + relation at the same time`` () =
    // Arrange
    createNodeAndExecute neo4jClient { FacebookId = "Parisian" }

    // Act
    let newHousehold: HouseholdNode = { Name = "Paris 10" }
    <@
        let parisian = declareNode<UserNode>
        matchNode parisian
        where (parisian.FacebookId = "Parisian")
        createRightRelation parisian declareRelationship<IsResidentOf> newHousehold
    @>
    |> executeWriteQuery neo4jClient.Cypher

    // Assert
    let myHouseholdsNames =
        <@
        let parisian = declareNode<UserNode>
        let household = declareNode<HouseholdNode>
        matchRightRelation parisian declareRelationship<IsResidentOf> household
        where (parisian.FacebookId = "Parisian")
        returnResults household
        @>
        |> executeReadQuery neo4jClient.Cypher
        |> Seq.map (fun hh -> hh.Name)

    CollectionAssert.Contains(myHouseholdsNames, "Paris 10", "Household names")

[<Test>]
let ``Create unique relationship`` () =
    // Arrange
    createNodeAndExecute neo4jClient { FacebookId = "Parisian" }

    // Act
    for _ in 1 .. 5 do
        let uniqueHouse: HouseholdNode = { Name = "UniqueHouse" }
        <@
        let person = declareNode<UserNode>
        matchNode person
        where (person.FacebookId = "Parisian")
        createUniqueRightRelation person declareRelationship<IsResidentOf> uniqueHouse
        @>
        |> executeWriteQuery neo4jClient.Cypher

    // Assert
    let nbOfHouses =
        <@
        let person = declareNode<UserNode>
        let house = declareNode<HouseholdNode>
        matchRightRelation person declareRelationship<IsResidentOf> house
        where (person.FacebookId = "Parisian")
        returnResults house
        @>
        |> executeReadQuery neo4jClient.Cypher
        |> Seq.length

    Assert.AreEqual(1, nbOfHouses, "Number of houses")

[<Test>]
let ``Delete relationship`` () =
    // Arrange
    createNodeAndExecute neo4jClient { Name = "Coloc de la joie" }

    for name in [ "Denis" ; "TT" ; "Opwal" ] do
        let user = { FacebookId = name }
        <@
        let hh = declareNode<HouseholdNode>
        matchNode hh
        where (hh.Name = "Coloc de la joie")
        createUniqueRightRelation user declareRelationship<IsResidentOf> hh
        @>
        |> executeWriteQuery neo4jClient.Cypher

    // Act
    <@
    let user = declareNode<UserNode>
    let rel = declareRelationship<IsResidentOf>
    matchRelation user rel declareNode<HouseholdNode>
    where (user.FacebookId = "Denis")
    deleteRelationship rel
    @>
    |> executeWriteQuery neo4jClient.Cypher

    // Assert
    let residentsOfColoc =
        <@
        let user = declareNode<UserNode>
        let hh = declareNode<HouseholdNode>
        matchRelation user declareRelationship<IsResidentOf> hh
        where (hh.Name = "Coloc de la joie")
        returnResults user
        @>
        |> executeReadQuery neo4jClient.Cypher
        |> Seq.map (fun user -> user.FacebookId)

    CollectionAssert.AreEquivalent([ "Opwal" ; "TT" ], residentsOfColoc, "Residents of the household")
