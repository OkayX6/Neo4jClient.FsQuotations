module Neo4jClient.FsQuotations.Tests.GrammarTests

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open NUnit.Framework
open Neo4jClient.FsQuotations

[<AutoOpen>]
module private GrammarHelpers =
    let rec getLastStatement expr =
        match expr with
        | Let(_, _, rest) -> getLastStatement rest
        | Sequential(_, rest) -> getLastStatement rest
        | _ -> expr

[<Test>]
let ``Match relations`` () =
    let queries = [
        <@
        let parisian = declareNode<UserNode>
        let household = declareNode<HouseholdNode>
        matchRightRelation parisian declareRelationship<IsResidentOf> household
        @>

        <@
        let rel = declareRelationship<IsResidentOf>
        matchLeftRelation declareNode<HouseholdNode> rel declareNode<UserNode>
        @>

        (let rel: IsResidentOf = { CustomHouseholdName = "foo" }
        <@
        let parisian = declareNode<UserNode>
        let household = declareNode<HouseholdNode>
        matchRelation parisian rel household
        @>)
    ]

    for q in queries do
        match getLastStatement q with
        | IsMatchRelationClause(_pat) -> ()
        | _ -> Assert.Fail("Statement not identified as a valid MATCH relation clause: {0}", q)
