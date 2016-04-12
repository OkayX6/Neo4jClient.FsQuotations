#r @"..\..\packages\Neo4jClient\lib\net45\Neo4jClient.dll"

#load "Cypher.fs"
      "Interpreter.fs"

open Neo4jClient.FsQuotations

type UserNode =
    { FacebookId: string }
    interface ICypherNode

type IsResidentOf() =
    interface ICypherRelationship

type HouseholdNode =
    { Name: string }
    interface ICypherNode

let readQuery =
    <@
    let u = declareNode<UserNode>()
    matchRelation u (declareRelationship<IsResidentOf>()) (declareNode<HouseholdNode>())
    where (u.FacebookId = "12345")
    returnResults u
    @>
