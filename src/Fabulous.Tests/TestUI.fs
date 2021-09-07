module TestUI

open System.Runtime.CompilerServices
open Fabulous
open Fabulous.ControlWidget

module A =
    module Text =
        let Text = Attributes.define<string> "Text" ""

    module TextStyle =
        let TextColor = Attributes.define<string> "TextColor" ""

    module Container =
        let Children =
            Attributes.createDefinitionWithConverter<seq<IWidget>, IWidget []>
                "Children"
                [||]
                Seq.toArray
                (fun a b -> Attributes.AttributeComparison.Different None)

    module Button =
        let Clicked =
            // TODO what is the proper way to represent that?
            Attributes.createDefinition<obj option>
                (fun a b -> Attributes.AttributeComparison.Different None)
                "_Clicked"
                None // TODO fix option

    module Automation =
        let AutomationId =
            Attributes.define<string> "AutomationId" ""

    module Runtime =
        let MapMsgFn =
            Attributes.createDefinition<obj option>
                (fun a b -> Attributes.AttributeComparison.Different None)
                "MapMsgFn"
                None // TODO fix option

//-------Widgets

type TestLabel(attributes, source) =
    let mutable attributes: Attribute [] = attributes

    member x.Color = A.TextStyle.TextColor.Get attributes
    member x.Text = A.Text.Text.Get attributes

    interface IViewNode with
        member this.ApplyDiff((diffs, attrs)) =
            printfn "new attrs: %A" attrs
            attributes <- attrs
            UpdateResult.Done

        member this.Source = source
        member this.Attributes = attributes

[<Struct>]
type LabelWidget<'msg>(attributes: Attribute []) =
    //    static do register<LabelWidget, TestLabel> () // TODO?

    static member inline Create(text: string) =
        LabelWidget<'msg>([| A.Text.Text.WithValue(text) |])

    interface IWidget with
        member this.Attributes = attributes
        member this.Source = typeof<LabelWidget<'msg>>

        member this.CreateView _ =
            TestLabel(attributes, typeof<LabelWidget<'msg>>) :> IViewNode


    interface IControlWidget with

        member this.Add(attribute) =
            addAttribute LabelWidget<'msg> attributes attribute

    interface ITypedWidget<'msg>

[<Extension>]
type LabelWidgetExtensions() =
    [<Extension>]
    static member inline textColor<'msg>(this: LabelWidget<'msg>, value: string) =
        this.AddAttribute(A.TextStyle.TextColor.WithValue(value))

///----------------

// emulation of MAUI button
type IPressable =
    abstract Press : unit -> unit

type TestButton(attributes, source, viewContext: ViewTreeContext) =
    let mutable attributes: Attribute [] = attributes
    member x.Text = A.Text.Text.Get attributes

    member x.Press() =
        match A.Button.Clicked.Get attributes with
        | Some msg -> viewContext.Dispatch msg
        | _ -> ()

    interface IViewNode with
        member this.ApplyDiff((diffs, attrs)) =
            //            printfn "new attrs: %A" attrs
            attributes <- attrs
            UpdateResult.Done

        member this.Source = source
        member this.Attributes = attributes



[<Struct>]
type ButtonWidget<'msg>(attributes: Attribute []) =
//    let source = typeof<ButtonWidget<'msg>>
    
    interface IWidget with
        member this.Attributes = attributes

        member this.CreateView(ctx) =
            TestButton(attributes, typeof<ButtonWidget<'msg>>, ctx) :> IViewNode

        member this.Source = typeof<ButtonWidget<'msg>>

    interface IControlWidget with
        member this.Add(attribute) =
            addAttribute ButtonWidget<'msg> attributes attribute



    interface ITypedWidget<'msg>

    static member inline Create(text: string, clicked: 'msg) =
        ButtonWidget<'msg>
            [|
                A.Text.Text.WithValue(text)
                A.Button.Clicked.WithValue(Some(box clicked))
            |]


[<Extension>]
type ButtonWidgetExtensions() =
    [<Extension>]
    static member inline textColor<'msg>(this: ButtonWidget<'msg>, value: string) =
        this.AddAttribute(A.TextStyle.TextColor.WithValue(value))

///----Stack----
type TestStack(attrs: Attribute [], source, ctx) =
    let mutable attributes: Attribute [] = attrs

    let mutable children: IViewNode [] =
        A.Container.Children.Get attrs
        |> Array.map(fun w -> w.CreateView(ctx))

    interface IViewContainer with
        member this.Children = children
        member this.UpdateChildren(upd) = children <- upd.Children

    interface IViewNode with
        member this.ApplyDiff((diffs, attrs)) =
            attributes <- attrs
            let childrenWidgets = A.Container.Children.Get attrs
            UpdateResult.UpdateChildren struct (this :> IViewContainer, childrenWidgets, ctx)

        member this.Source = source
        member this.Attributes = attributes




[<Struct>]
type StackLayoutWidget<'msg>(attributes: Attribute []) =
    static member inline Create<'m, 'a when 'a :> seq<ITypedWidget<'m>>>(children: 'a) =
        StackLayoutWidget<'m>(
            [|
                // TODO what is the right type conversion here? Is there one needed at all?
                A.Container.Children.WithValue(children |> Seq.map(fun c -> c :> IWidget))
            |]
        )


    interface IWidget with
        member this.Attributes = attributes

        member this.CreateView(ctx) =
            TestStack(attributes, typeof<StackLayoutWidget<'msg>>, ctx) :> IViewNode

        member this.Source = typeof<StackLayoutWidget<'msg>>


    interface IControlWidget with
        member this.Add(attribute) =
            addAttribute StackLayoutWidget<'msg> attributes attribute

    interface ITypedWidget<'msg>

//--------

// Note that it is ok it to be not a [<Struct>] because it is not intended to be copied
type MapDispatchWidget<'msg, 'childMsg>(mapFn: 'childMsg -> 'msg, child: ITypedWidget<'childMsg>) =
    interface IWidget with
        member this.Attributes = child.Attributes

        member this.CreateView(ctx) =
            let dispatch (msg: obj) =
                ctx.Dispatch(unbox<'childMsg> msg |> mapFn |> box)

            child.CreateView({ ctx with Dispatch = dispatch })

        member this.Source = child.Source

    interface ITypedWidget<'msg>

module Widget =
    let inline map (fn: 'a -> 'b) (widget: ITypedWidget<'a>) : ITypedWidget<'b> =
        MapDispatchWidget(fn, widget) :> ITypedWidget<'b>


//----------

[<Extension>]
type SharedExtensions() =
    [<Extension>]
    static member inline automationId(this: #IControlWidget, value: string) =
        this.AddAttribute(A.Automation.AutomationId.WithValue(value))

    [<Extension>]
    static member inline cast<'msg>(this: ITypedWidget<'msg>) = this


///----------------
[<AbstractClass; Sealed>]
type View private () =
    static member inline Label<'msg>(text) = LabelWidget<'msg>.Create text
    static member inline Button<'msg>(text, msg) = ButtonWidget<'msg>.Create (text, msg)
    static member inline Stack<'m, 'a when 'a :> seq<ITypedWidget<'m>>>(children:'a) = StackLayoutWidget<_>.Create<'m, 'a> (children)

///------------------
type StatefulView<'arg, 'model, 'msg, 'view when 'view :> IWidget> =
    {
        State: RunnerKey option
        Init: 'arg -> 'model
        Update: 'msg -> 'model -> 'model
        View: 'model -> 'view
    }

    interface IStatefulWidget<'arg, 'model, 'msg, 'view> with
        member x.State = x.State
        member x.Init(arg) = x.Init arg
        member x.Update(msg, model) = x.Update msg model
        member x.View(model) = x.View model

module StatefulWidget =
    let mkSimpleView init update view : StatefulView<_, _, _, _> =
        {
            State = None
            Init = init
            Update = update
            View = view
        }


module Run =

    let rec traverse (viewNode: IViewNode) test =
        let inline testToOption node = if test node then Some node else None

        // TODO refactor using only containers as special cases
        match viewNode with
        | :? TestButton
        | :? TestLabel -> testToOption viewNode
        | :? TestStack as stack ->
            let children = (stack :> IViewContainer).Children

            let initial = testToOption viewNode

            children
            |> Array.fold(fun res child -> res |> Option.orElse(testToOption child)) initial
        | _ -> None

    type ViewTree =
        {
            FindByAutomationId: string -> IViewNode option
        }

    type Instance<'arg, 'model, 'msg, 'widget when 'widget :> IWidget>
        (
            program: IStatefulWidget<'arg, 'model, 'msg, 'widget>
        ) =
        let mutable state: ('model * IViewNode) option = None

        member private x.viewContext: ViewTreeContext =
            {
                Dispatch = fun msg -> unbox<'msg> msg |> x.ProcessMessage
            }

        member x.ProcessMessage(msg: 'msg) =
            match state with
            | None -> ()
            | Some (m, viewNode) ->
                let newModel = program.Update(msg, m)
                let newWidget = program.View(newModel)

                // is it better to have a Kind prop instead
                // basically we care if it is exactly the same widget type as it was before
                // TODO support mount + unmount
                // TODO possibly introduce some notion of a platform/runtime context
                // that can mount and unmount nodes
                if newWidget.Source <> viewNode.Source then
                    failwith "type mismatch!"

                state <- Some(newModel, viewNode)

                Reconciler.update viewNode newWidget.Attributes x.viewContext

                ()

        member x.Start(arg: 'arg) =
            let model = (program.Init(arg))
            let widget = program.View(model)
            let view = widget.CreateView(x.viewContext)

            state <- Some(model, view)

            {
                FindByAutomationId =
                    (fun id ->
                        traverse
                            view
                            (fun node ->
                                let maybeId =
                                    A.Automation.AutomationId.TryGet node.Attributes

                                match maybeId with
                                | Some automationId -> automationId = id
                                | None -> false))
            }
