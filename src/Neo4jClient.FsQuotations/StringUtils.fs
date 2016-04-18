[<AutoOpen>]
module internal Neo4jClient.FsQuotations.StringUtils

open System

let inline join separator (values: #seq<string>) = String.Join(separator, values)