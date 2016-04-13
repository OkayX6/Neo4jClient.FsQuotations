﻿namespace Neo4jClient.FsQuotations

open System
open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Neo4jClient.Cypher

type Neo4jKeyAttribute() =
    inherit Attribute()

type NodeMatch<'TKey> = 
    { Key : 'TKey
      TypeInfo : Type }
    static member Create<'T>(key: 'TKey) =
        { Key = key
          TypeInfo = typeof<'T> }

[<RequireQualifiedAccess>]
module Cypher =
    let inline raiseNeo4jNodeTypeExceptionFromType (typeinfo: Type) =
        raise (ArgumentException(sprintf "The '%s' type must define a property with 'Neo4jKeyAttribute'" typeinfo.FullName))

    let inline raiseNeo4jNodeTypeException<'T> () = raiseNeo4jNodeTypeExceptionFromType typeof<'T>

    let inline private tryFindNeo4jKey (entity: 'T) =
        let properties = typeof<'T>.GetProperties()
        properties
        |> Array.tryPick (fun prop ->
            prop.CustomAttributes
            |> Seq.tryPick (fun attr ->
                if attr.AttributeType = typeof<Neo4jKeyAttribute> then
                    Some (prop.Name, prop.GetValue(entity))
                else
                    None
            )
        )

    let inline tryGetNeo4jKeyName (typeInfo: Type) =
        let properties = typeInfo.GetProperties()
        properties
        |> Array.tryPick (fun prop ->
            prop.CustomAttributes
            |> Seq.tryPick (fun attr ->
                if attr.AttributeType = typeof<Neo4jKeyAttribute> then
                    Some prop.Name
                else
                    None
            )
        )

    let inline getNeo4jKeyName (typeInfo: Type) =
        let properties = typeInfo.GetProperties()
        properties
        |> Array.pick (fun prop ->
            prop.CustomAttributes
            |> Seq.tryPick (fun attr ->
                if attr.AttributeType = typeof<Neo4jKeyAttribute> then
                    Some prop.Name
                else
                    None
            )
        )

    let inline private tryGenerateMatchNode (node: 'T) nodeName =
        tryFindNeo4jKey node
        |> Option.map (fun (keyName, keyValue) ->
            sprintf "(%s:%s {%s:'%O'})" nodeName typeof<'T>.Name keyName keyValue
        )

    let inline private tryGenerateMatchExpr (data: NodeMatch<'TKey>) nodeName =
        tryGetNeo4jKeyName data.TypeInfo
        |> Option.map (fun keyName ->
            sprintf "%s:%s {%s:'%O'}" nodeName data.TypeInfo.Name keyName data.Key
        )

    let createNode (node: 'T) (query: ICypherFluentQuery) =
        let argName = "arg"
        let createText = sprintf "(node:%s {%s})" typeof<'T>.Name argName
        query.Create(createText)
             .WithParam(argName, node)

    // TODO denisok: enforce INeo4jRelationship
    let createNodeAndUniquelyRelateTo (sourceNode: NodeMatch<'TSourceKey>) (relationship: 'TRelationship) (targetNodeToCreate: 'TTargetNode) (query: ICypherFluentQuery) =
        let sourceNodeMatchExpr' = tryGenerateMatchExpr sourceNode "sourceNode"
        match sourceNodeMatchExpr' with
        | Some sourceNodeMatchExpr ->
            let relationshipArgName = "relationshipArg"
            let targetNodeArgName = "targetNodeArg"

            query.Match(sprintf "(%s)" sourceNodeMatchExpr)
                 .CreateUnique(
                    sprintf "(sourceNode)-[:%s {%s}]->(targetNodeToCreate:%s {%s})"
                        typeof<'TRelationship>.Name
                        relationshipArgName
                        typeof<'TTargetNode>.Name
                        targetNodeArgName)
                 .WithParams(
                    dict [
                        relationshipArgName, box relationship
                        targetNodeArgName, box targetNodeToCreate
                    ])
        | None -> raiseNeo4jNodeTypeException<'TTargetNode>()

    let deleteRelationship (sourceNode: NodeMatch<'TSourceKey>) (relationshipType: Type) (targetNode: NodeMatch<'TTargetKey>) (query: ICypherFluentQuery) =
        let sourceMatchExpr' = tryGenerateMatchExpr sourceNode "sourceNode"
        let targetMatchExpr' = tryGenerateMatchExpr targetNode "targetNode"
        match sourceMatchExpr', targetMatchExpr' with
        | Some sourceMatchExpr, Some targetMatchExpr ->
            query.Match(sprintf "(%s)-[rel:%s]->(%s)" sourceMatchExpr relationshipType.Name targetMatchExpr)
                 .Delete("rel")
        | None, _ -> raiseNeo4jNodeTypeExceptionFromType sourceNode.TypeInfo
        | _, _ -> raiseNeo4jNodeTypeExceptionFromType targetNode.TypeInfo

    let mergeNode (node: 'T) (query: ICypherFluentQuery) =
        match tryFindNeo4jKey node with
        | Some(propName, propValue) ->
            let argName = "arg"
            let mergeText = sprintf "(node:%s {%s: '%O'})" typeof<'T>.Name propName propValue
            let setText = sprintf "node = {%s}" argName

            query.Merge(mergeText)
                 .OnCreate()
                 .Set(setText)
                 .WithParam(argName, node)
        | None -> raiseNeo4jNodeTypeException<'T>()

    let inline matchNodes<'T> (query: ICypherFluentQuery) =
        query.Match(sprintf "(node:%s)" typeof<'T>.Name)

    let inline returnNodes<'T> (query: ICypherFluentQuery) =
        <@ Func<_, _>(fun (node: ICypherResultItem) -> node.As<'T>()) @>
        |> QuotationToLambdaExpression
        |> query.Return

    let inline getNodes<'T> (query: ICypherFluentQuery) =
        query
        |> matchNodes<'T>
        |> returnNodes<'T>

    let inline executeWithoutResults (query: ICypherFluentQuery) =
        query.ExecuteWithoutResults()

    let inline asyncExecuteWithoutResults (query: ICypherFluentQuery) =
        query.ExecuteWithoutResultsAsync()
        |> Async.AwaitTask

    let inline getResults (query: ICypherFluentQuery<'T>) = query.Results
    let inline getResultsAsync (query: ICypherFluentQuery<'T>) = Async.AwaitTask query.ResultsAsync