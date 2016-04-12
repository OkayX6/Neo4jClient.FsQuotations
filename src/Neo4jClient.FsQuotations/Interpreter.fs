namespace Neo4jClient.FsQuotations

open System
open System.Reflection
open System.Linq.Expressions
open Microsoft.FSharp.Linq.RuntimeHelpers
open Microsoft.FSharp.Linq.RuntimeHelpers.LeafExpressionConverter
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type ICypherNode = interface end
type ICypherRelationship = interface end

type CyMatch = CyMatch
type CyWhere = CyWhere
type CyReturn<'T> = | CyReturn
type CyReadQuery() =
    static member Create
        (matchClause: CyMatch,
         matchWhere: CyWhere,
         optMatchClause: CyMatch,
         optMatchWhere: CyWhere,
         returnClause: CyReturn<'T>) = CyReadQuery()

type MyUserNode = 
    { FacebookId: string }
    interface ICypherNode

type IsResidentOf() =
    interface ICypherRelationship

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///

[<AutoOpen>]
module QuotationInterpreter = 
    /// Returns 42
    ///
    /// ## Parameters
    ///  - `num` - whatever
    let hello num = 42

    let declareNode<'T when 'T :> ICypherNode> () = Unchecked.defaultof<'T>
    let declareRelationship<'T when 'T :> ICypherRelationship> () = Unchecked.defaultof<'T>
    let matchNode (x: #ICypherNode) = ()
    let matchRelation (x: #ICypherNode) (rel: #ICypherRelationship) (y: #ICypherNode) = ()
    let optionalMatchNode (x: #ICypherNode) = ()
    let optionalMatchRelation (x: #ICypherNode) (rel: #ICypherRelationship) (y: #ICypherNode) = ()
    let where (boolExpr: bool) = ()
    let returnResults (x: 'T): CyReturn<'T> = CyReturn

    let executeMatch (q: Expr) =
        match q with
        | Sequential(Call(None, methodInfo, _), rest)
            when methodInfo.Name = "matchNode" || methodInfo.Name = "matchRelation" ->
            printfn "%s" methodInfo.Name

    let rec execute (q: Expr) =
        match q with
        | Let(var, expr, rest) when var.Name.Length > 0 ->
            match expr with
            | Patterns.Call(None, methodInfo, []) when methodInfo.Name = "declareNode" ->
                printfn "Declare node: %s (type: %s)" var.Name var.Type.Name
                match rest with
                | Let(_) -> execute rest
                | _ -> executeMatch rest
            | Patterns.Call(None, methodInfo, []) when methodInfo.Name = "declareNode" ->
                printfn "Declare relationship: %s (type: %s)" var.Name var.Type.Name
                match rest with
                | Let(_) -> execute rest
                | _ -> executeMatch rest
            | _ -> printfn "Only calls to 'declareNode' or 'declareRelationship' are allowed in a 'let' construct"
        | Let(_) -> printfn "Invalid 'let' expression: bounded value must have a name"

        | _ -> printfn "Unknown expression"
