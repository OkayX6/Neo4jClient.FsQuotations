namespace Neo4jClient.FsQuotations

open System
open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Neo4jClient.Cypher

type Neo4jKeyAttribute() =
    inherit Attribute()

[<NoComparison>]
type internal NodeMatch<'TKey> = 
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

    let internal tryFindNeo4jKey (entity: 'T) =
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

    let internal findNeo4jKey (entity: 'T) =
        entity
        |> tryFindNeo4jKey
        |> Option.get

    let tryGetNeo4jKeyName (typeInfo: Type) =
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

    let getNeo4jKeyName (typeInfo: Type) =
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

    let inline private tryGenerateMatchExpr (data: NodeMatch<'TKey>) nodeName =
        tryGetNeo4jKeyName data.TypeInfo
        |> Option.map (fun keyName ->
            sprintf "%s:%s {%s:'%O'}" nodeName data.TypeInfo.Name keyName data.Key
        )

    let internal deleteRelationship (sourceNode: NodeMatch<'TSourceKey>) (relationshipType: Type) (targetNode: NodeMatch<'TTargetKey>) (query: ICypherFluentQuery) =
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
