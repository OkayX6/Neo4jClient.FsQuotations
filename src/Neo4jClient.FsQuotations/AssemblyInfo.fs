namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Neo4jClient.FsQuotations")>]
[<assembly: AssemblyProductAttribute("Neo4jClient.FsQuotations")>]
[<assembly: AssemblyDescriptionAttribute("Neo4jClient with F# quotations")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
    let [<Literal>] InformationalVersion = "1.0"
