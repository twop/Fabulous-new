module TestUI

open System.Runtime.CompilerServices
open Fabulous
open Fabulous.ControlWidget

module A =
    module Text =
        let Text = Attributes.define<string> "Text" ""

    module TextStyle =
        let TextColor = Attributes.define<string> "TextColor" ""

type TestLabel(attributes, widget) =
    let widget = widget
    let mutable attributes: Attribute [] = attributes

    member x.Color = A.TextStyle.TextColor.Get attributes
    member x.Text = A.Text.Text.Get attributes

    interface IViewNode with
        member this.ApplyDiff((diffs, attrs)) =
            printfn "new attrs: %A" attrs
            attributes <- attrs
            UpdateResult.Done

        member this.Widget = widget

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
            TestLabel(attributes, this) :> IViewNode

[<Extension>]
type LabelWidgetExtensions() =
    [<Extension>]
    static member inline textColor(this: LabelWidget, value: string) =
        this.AddAttribute(A.TextStyle.TextColor.WithValue(value))

[<AbstractClass; Sealed>]
type View private () =
    static member inline Label(text) = LabelWidget.Create(text)


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
                if newWidget.GetType() <> viewNode.Widget.GetType() then
                    failwith "type mismatch!"

                state <- Some(newModel, viewNode)

                Reconciler.update viewNode newWidget.Attributes

                ()

        member x.Start(arg: 'arg) =
            let model = (program.Init(arg))
            let widget = program.View(model)
            let view = widget.CreateView()
            state <- Some (model, view)
            view
