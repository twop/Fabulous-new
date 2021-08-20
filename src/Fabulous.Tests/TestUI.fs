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
    let mutable widget = widget
    let mutable attributes: Attribute [] = attributes
    
    member x.Color = A.TextStyle.TextColor.Get attributes
    member x.Text = A.Text.Text.Get attributes
    
    interface Reconciler.IViewNode with
        member this.ApplyDiff((w, diff)) =
            widget <- w
            attributes <- w.Attributes
            Reconciler.Done
            
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

        member this.CreateView() = box (TestLabel(attributes, this))

[<Extension>]
type LabelWidgetExtensions() =
    [<Extension>]
    static member inline textColor(this: LabelWidget, value: string) =
        this.AddAttribute(A.TextStyle.TextColor.WithValue(value))

[<AbstractClass; Sealed>]
type View private () =
    static member inline Label(text) = LabelWidget.Create(text)
