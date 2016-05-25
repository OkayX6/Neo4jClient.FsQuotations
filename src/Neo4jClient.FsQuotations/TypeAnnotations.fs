namespace Neo4jClient.FsQuotations

open System

type INeo4jNode = interface end
type INeo4jRelationship = interface end
type Neo4jKeyAttribute() = inherit Attribute()

[<RequireQualifiedAccess>]
module internal Cypher =
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