namespace Neo4jClient.FsQuotations

/// This module expose the grammar to write a Cypher queries in quotations.
///
/// ## Example
///
///     <@
///     let n = declareNode<MyNode>
///     returnResults n
///     @>
///
[<AutoOpen>]
module CypherGrammar =
    type CypherResult<'T> = internal | CypherResult

    /// Declares a node of a specific type
    ///
    /// ## Type parameters
    ///  - `'T` - the type of the node
    let declareNode<'T when 'T :> INeo4jNode> = Unchecked.defaultof<'T>

    /// Declares a relationship of a specific type
    ///
    /// ## Type parameters
    ///  - `'T` - the type of the relationship
    let declareRelationship<'T when 'T :> INeo4jRelationship> = Unchecked.defaultof<'T>

    /// Matches a node with a specific type like: "MATCH (n: 'T)"
    ///
    /// ## Type parameters
    ///  - `'T` - the type of the node
    let matchNode (_: #INeo4jNode) = ()

    let matchRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let matchLeftRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let matchRightRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()
    let optionalMatchNode (_: #INeo4jNode) = ()
    let optionalMatchRelation (_: #INeo4jNode) (_: #INeo4jRelationship) (_: #INeo4jNode) = ()

    let where (_:bool) = ()
    let isNotNull (_:bool) = false

    let createNode (_: 'TNode when 'TNode :> INeo4jNode) = ()
    let createLeftRelation (_:#INeo4jNode) (_:#INeo4jRelationship) (_:#INeo4jNode) = ()
    let createRightRelation (_:#INeo4jNode) (_:#INeo4jRelationship) (_:#INeo4jNode) = ()
    let createUniqueLeftRelation (_:#INeo4jNode) (_:#INeo4jRelationship) (_:#INeo4jNode) = ()
    let createUniqueRightRelation (_:#INeo4jNode) (_:#INeo4jRelationship) (_:#INeo4jNode) = ()
    let mergeNode (_:#INeo4jNode) = ()
    let mergeLeftRelation (_:#INeo4jNode) (_:#INeo4jRelationship) (_:#INeo4jNode) = ()
    let mergeRightRelation (_:#INeo4jNode) (_:#INeo4jRelationship) (_:#INeo4jNode) = ()
    let deleteNode (_: 'T when 'T :> INeo4jNode) = ()
    let deleteRelationship (_: 'T when 'T :> INeo4jRelationship) = ()

    let returnResults (_: 'T): CypherResult<'T> = CypherResult
