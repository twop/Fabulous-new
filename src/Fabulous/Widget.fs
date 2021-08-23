namespace Fabulous


open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Struct>]
type RunnerKey = RunnerKey of int

[<Struct>]
type AttributeKey = AttributeKey of int

type Attribute = { Key: AttributeKey; Value: obj }

module Attributes =
    type IAttributeDefinition =
        interface
        end

    [<Struct>]
    [<RequireQualifiedAccess>]
    type AttributeComparison =
        | Same
        | NotSure
        | Different of data: obj option

    type AttributeComparer<'modelType> = 'modelType -> 'modelType -> AttributeComparison

    type AttributeDefinition<'inputType, 'modelType> =
        {
            Key: AttributeKey
            Name: string
            DefaultValue: 'modelType
            Convert: 'inputType -> 'modelType
            Compare: AttributeComparer<'modelType>
        }
        interface IAttributeDefinition

        member x.WithValue(value) =
            {
                Key = x.Key
                Value = x.Convert(value)
            }

        member x.TryGet(attributes: Attribute []) =
            let attr =
                attributes
                |> Array.tryFind (fun attr -> attr.Key = x.Key)

            match attr with
            | Some attr -> Some (unbox<'modelType> attr.Value)
            | None -> None
            
        member x.Get(attributes: Attribute []) =
            x.TryGet attributes |> Option.defaultValue x.DefaultValue
            
//            let attr =
//                attributes
//                |> Array.tryFind (fun attr -> attr.Key = x.Key)
//
//            match attr with
//            | Some attr -> unbox<'modelType> attr.Value
//            | None -> x.DefaultValue
        

    let private _attributes =
        Dictionary<AttributeKey, IAttributeDefinition>()

    let createDefinitionWithConverter<'inputType, 'modelType>
        (name: string)
        (defaultValue: 'modelType)
        (convert: 'inputType -> 'modelType)
        (compare: 'modelType -> 'modelType -> AttributeComparison)
        : AttributeDefinition<'inputType, 'modelType> =

        let key = AttributeKey(_attributes.Count + 1)

        let definition =
            {
                Key = key
                Convert = convert
                Name = name
                DefaultValue = defaultValue
                Compare = compare
            }

        _attributes.Add(key, definition)
        definition

    let createDefinition<'T> comparer name defaultValue =
        createDefinitionWithConverter<'T, 'T> name defaultValue id comparer

    let inline define<'T when 'T: equality> name defaultValue =
        createDefinition<'T>
            (fun a b ->
                if a = b then
                    AttributeComparison.Same
                else
                    (AttributeComparison.Different None))
            name
            defaultValue

    [<RequireQualifiedAccess>]
    type AttributeDiff =
        | Different of struct (Attribute * Attribute * obj option)
        | Added of Attribute
        | Removed of Attribute

    let compareAttributes (prev: Attribute []) (next: Attribute []) : AttributeDiff list =
        [AttributeDiff.Added next.[0]]


/// Base logical element
type IWidget =
    abstract CreateView : unit -> IViewNode
    abstract Attributes : Attribute []

and IViewNode =
    // is this needed?
    // maybe have something like WidgetType and Attributes separately?
    abstract member Widget : IWidget
    abstract member ApplyDiff : (Attributes.AttributeDiff list * Attribute []) -> UpdateResult

and [<RequireQualifiedAccess>] UpdateResult =
    | Done
    | UpdateNext of (struct (IViewNode * Attribute [])) list // this is the way to update it's children

module ControlWidget =
    type IControlWidget =
        inherit IWidget
        abstract Add : Attribute -> IControlWidget

    type Handler =
        {
            TargetType: Type
            Create: Attribute [] -> obj
        }

    let private _handlers = Dictionary<Type, Handler>()

    let registerWithCustomCtor<'Builder, 'T> (create: Attribute [] -> 'T) =
        if not (_handlers.ContainsKey(typeof<'Builder>)) then
            _handlers.[typeof<'Builder>] <-
                {
                    TargetType = typeof<'T>
                    Create = create >> box
                }

    let register<'Builder, 'T when 'T: (new : unit -> 'T)> () =
        registerWithCustomCtor<'Builder, 'T> (fun _ -> new 'T())


    let inline addAttribute (fn: Attribute [] -> #IControlWidget) (attribs: Attribute []) (attr: Attribute) =
        let attribs2 = Array.zeroCreate (attribs.Length + 1)
        Array.blit attribs 0 attribs2 0 attribs.Length
        //        printfn "%A %A" attribs2.Length attribs.Length
        attribs2.[attribs.Length] <- attr
        (fn attribs2) :> IControlWidget

    [<Extension>]
    type IControlWidgetExtensions() =
        [<Extension>]
        static member inline AddAttribute<'T when 'T :> IControlWidget>(this: 'T, attr: Attribute) =
            this.Add(attr) :?> 'T



/// Logical element without state able to generate a logical tree composed of child widgets
type IStatelessWidget<'view when 'view :> IWidget> =
    abstract View : Attribute [] -> 'view

/// Logical element with MVU state able to generate a logical tree composed of child widgets
type IStatefulWidget<'arg, 'model, 'msg, 'view when 'view :> IWidget> =
    abstract State : RunnerKey option
    abstract Init : 'arg -> 'model
    abstract Update : 'msg * 'model -> 'model
    abstract View : 'model -> 'view


//-----Update sketch------
module Reconciler =
    let rec update (node: IViewNode) (attributes: Attribute[]) =
        let diff =
            Attributes.compareAttributes node.Widget.Attributes attributes

        if List.isEmpty diff then
            ()
        else
            match node.ApplyDiff(diff, attributes) with
            | UpdateResult.Done -> ()
            | UpdateResult.UpdateNext updateRequests ->
                for struct (node, attrs) in updateRequests do
                    update node attrs



// 1. compare attributes for control and widget
// 2. apply diff (should be a method on control). e.g control.ApplyDiff(diff)
// 3. apply returns UpdateResult
// 4. if we have 'Done' then exit
// 5. if we have 'UpdateNext' go through the list and recursively call update on each element

// Questions
// 1. should adding/removing children should be done here as well?
// 2. Should 'mounting' views done by core framework or should be part of MAUI? I think the latter is fine
// 3. Should widgets have 1st class parent -> child relationships like in react?
// Meaning that we can fully control creations of new controls via core
