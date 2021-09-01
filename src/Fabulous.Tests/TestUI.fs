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
            Attributes.createDefinition<obj option>(fun a b -> Attributes.AttributeComparison.Different None) "_Clicked" None // TODO fix option

    module Automation =
        let AutomationId =
            Attributes.define<string> "AutomationId" ""

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
type LabelWidget(attributes: Attribute []) =
    //    static do register<LabelWidget, TestLabel> () // TODO?

    static member inline Create(text: string) =
        LabelWidget([| A.Text.Text.WithValue(text) |])

    interface IControlWidget with
        member this.Attributes = attributes

        member this.Add(attribute) =
            addAttribute LabelWidget attributes attribute

        member this.CreateView() =
            TestLabel(attributes, typeof<LabelWidget>) :> IViewNode

[<Extension>]
type LabelWidgetExtensions() =
    [<Extension>]
    static member inline textColor(this: LabelWidget, value: string) =
        this.AddAttribute(A.TextStyle.TextColor.WithValue(value))

    [<Extension>]
    static member inline automationId(this: LabelWidget, value: string) =
        this.AddAttribute(A.Automation.AutomationId.WithValue(value))


///----------------

// emulation of MAUI button
type IPressable =
    abstract Press : unit -> unit

type TestButton(attributes, source) =
    let mutable attributes: Attribute [] = attributes
    member x.Text = A.Text.Text.Get attributes
    //    member x.ClickMsg = A.Button.Clicked.Get attributes

    // this should be set by runtime
    member val Dispatch: (obj -> unit) option = None with get, set

    member x.Press() =
        match (x.Dispatch, A.Button.Clicked.Get attributes) with
        | Some dispatch, Some msg -> dispatch msg
        | _ -> ()

    interface IViewNode with
        member this.ApplyDiff((diffs, attrs)) =
            //            printfn "new attrs: %A" attrs
            attributes <- attrs
            UpdateResult.Done

        member this.Source = source
        member this.Attributes = attributes

[<Struct>]
type ButtonWidget(attributes: Attribute []) =

    interface IControlWidget with
        member this.Attributes = attributes

        member this.Add(attribute) =
            addAttribute ButtonWidget attributes attribute

        member this.CreateView() =
            TestButton(attributes, typeof<ButtonWidget>) :> IViewNode

    static member inline Create(text: string, clicked: (#obj)) =
        ButtonWidget [| A.Text.Text.WithValue(text)
                        A.Button.Clicked.WithValue(Some(box clicked)) |]


[<Extension>]
type ButtonWidgetExtensions() =
    [<Extension>]
    static member inline textColor(this: ButtonWidget, value: string) =
        this.AddAttribute(A.TextStyle.TextColor.WithValue(value))

    [<Extension>]
    static member inline automationId(this: ButtonWidget, value: string) =
        this.AddAttribute(A.Automation.AutomationId.WithValue(value))

///----Stack----
type TestStack(attrs: Attribute [], source) =
    let mutable attributes: Attribute [] = attrs

    let mutable children: IViewNode [] =
        A.Container.Children.Get attrs
        |> Array.map(fun w -> w.CreateView())

    interface IViewContainer with
        member this.Children = children
        member this.UpdateChildren(upd) = children <- upd.Children
        
        
    interface IViewNode with
        member this.ApplyDiff((diffs, attrs)) =
            attributes <- attrs
            let childrenWidgets = A.Container.Children.Get attrs
            UpdateResult.UpdateChildren struct (this :> IViewContainer, childrenWidgets)

        member this.Source = source
        member this.Attributes = attributes




[<Struct>]
type StackLayoutWidget(attributes: Attribute []) =
    static member inline Create(children: seq<#IWidget>) =
        StackLayoutWidget(
            [|
                // TODO what is the right type conversion here? Is there one needed at all?
                A.Container.Children.WithValue(children |> Seq.map(fun c -> c :> IWidget))
            |]
        )

    interface IControlWidget with
        member this.Attributes = attributes

        member this.Add(attribute) =
            addAttribute StackLayoutWidget attributes attribute

        member this.CreateView() =
            TestStack(attributes, typeof<StackLayoutWidget>) :> IViewNode

[<Extension>]
type StackLayoutWidgetExtensions() =
    [<Extension>]
    static member inline automationId(this: StackLayoutWidget, value: string) =
        this.AddAttribute(A.Automation.AutomationId.WithValue(value))

///----------------
[<AbstractClass; Sealed>]
type View private () =
    static member inline Label(text) = LabelWidget.Create(text)
    static member inline Button(text, msg) = ButtonWidget.Create(text, msg)
    static member inline Stack(children) = StackLayoutWidget.Create(children)

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

        member x.ProcessMessage(msg) =
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
                if newWidget.GetType() <> viewNode.Source then
                    failwith "type mismatch!"

                state <- Some(newModel, viewNode)

                Reconciler.update viewNode newWidget.Attributes

                ()

        member x.Start(arg: 'arg) =
            let model = (program.Init(arg))
            let widget = program.View(model)
            let view = widget.CreateView()

            match view with
            // TODO generalize this code somehow
            | :? TestButton as btn -> btn.Dispatch <- Some(fun msgObj -> x.ProcessMessage(unbox<'msg> msgObj))
            | _ -> ()


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
