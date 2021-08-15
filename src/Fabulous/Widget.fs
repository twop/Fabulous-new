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

    type AttributeDefinition<'inputType, 'modelType> =
        {
            Key: AttributeKey
            Name: string
            DefaultValue: 'modelType
            Convert: 'inputType -> 'modelType
        }
        interface IAttributeDefinition

        member x.WithValue(value) =
            {
                Key = x.Key
                Value = x.Convert(value)
            }

        member x.Get(attributes: Attribute []) =
            let attr =
                attributes
                |> Array.tryFind (fun attr -> attr.Key = x.Key)

            match attr with
            | Some attr -> unbox<'modelType> attr.Value
            | None -> x.DefaultValue

    let private _attributes =
        Dictionary<AttributeKey, IAttributeDefinition>()

    let createDefinitionWithConverter<'inputType, 'modelType>
        (name: string)
        (defaultValue: 'modelType)
        (convert: 'inputType -> 'modelType)
        : AttributeDefinition<'inputType, 'modelType> =

        let key = AttributeKey(_attributes.Count + 1)

        let definition =
            {
                Key = key
                Convert = convert
                Name = name
                DefaultValue = defaultValue
            }

        _attributes.Add(key, definition)
        definition

    let createDefinition<'T> name defaultValue =
        createDefinitionWithConverter<'T, 'T> name defaultValue id



/// Base logical element
type IWidget =
    abstract CreateView : unit -> obj

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
        attribs2.[attribs.Length + 1] <- attr
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
    abstract View : 'model * Attribute [] -> 'view
