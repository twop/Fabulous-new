namespace Fabulous.Maui

open System.Runtime.CompilerServices
open Fabulous
open Microsoft.Maui
open Microsoft.Maui
open Microsoft.Maui.Graphics
open Fabulous.ControlWidget
open Microsoft.Maui.Layouts


// ATTRIBUTES

// MAUI CONTROLS

module A = Attributes

type FabulousApplication() =
    interface IApplication with
        member this.ThemeChanged() = failwith "todo"
        member this.CreateWindow(activationState) = failwith "todo"
        member this.Windows = failwith "todo"

type FabulousWindow() =
    member val Attributes: Attribute [] = [||]

    interface IWindow with
        member this.Activated() = failwith "todo"
        member this.Created() = failwith "todo"
        member this.Deactivated() = failwith "todo"
        member this.Destroying() = failwith "todo"
        member this.Resumed() = failwith "todo"
        member this.Stopped() = failwith "todo"
        member this.Content = failwith "todo"
        member val Handler = null with get, set
        member this.Parent = failwith "todo"

        member this.Title = A.Window.Title.Get this.Attributes

type FabulousStackLayout() =

    member val Attributes: Attribute [] = [||]

    interface IStackLayout with
        member this.Add(item: IView) = failwith "todo"
        member this.AnchorX = failwith "todo"
        member this.AnchorY = failwith "todo"
        member this.Arrange(bounds: Rectangle) = failwith "todo"
        member this.AutomationId = failwith "todo"
        member this.Background = failwith "todo"
        member this.Clear() = failwith "todo"
        member this.Clip = failwith "todo"
        member this.Contains(item: IView) = failwith "todo"
        member this.CopyTo(array: IView [], arrayIndex: int) = failwith "todo"
        member this.Count = failwith "todo"
        member this.DesiredSize = failwith "todo"
        member this.FlowDirection = failwith "todo"

        member this.Frame
            with get (): Rectangle = raise (System.NotImplementedException())
            and set (v: Rectangle): unit = raise (System.NotImplementedException())

        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<IView> =
            raise (System.NotImplementedException())

        member this.GetEnumerator() : System.Collections.IEnumerator =
            raise (System.NotImplementedException())

        member this.Handler
            with get (): IElementHandler = raise (System.NotImplementedException())
            and set (v: IElementHandler): unit = raise (System.NotImplementedException())

        member this.Handler
            with get (): IViewHandler = raise (System.NotImplementedException())
            and set (v: IViewHandler): unit = raise (System.NotImplementedException())

        member this.Height = failwith "todo"
        member this.HorizontalLayoutAlignment = failwith "todo"
        member this.IgnoreSafeArea = failwith "todo"
        member this.IndexOf(item: IView) = failwith "todo"
        member this.Insert(index: int, item: IView) = failwith "todo"
        member this.InvalidateArrange() = failwith "todo"
        member this.InvalidateMeasure() = failwith "todo"
        member this.IsEnabled = failwith "todo"
        member this.IsReadOnly = failwith "todo"

        member this.Item
            with get (index: int): IView = raise (System.NotImplementedException())
            and set (index: int) (v: IView): unit = raise (System.NotImplementedException())

        member this.LayoutHandler = failwith "todo"
        member this.LayoutManager = failwith "todo"
        member this.Margin = failwith "todo"
        member this.Measure(widthConstraint: float, heightConstraint: float) = failwith "todo"
        member this.Opacity = failwith "todo"
        member this.Padding = failwith "todo"
        member this.Parent: IElement = raise (System.NotImplementedException())
        member this.Parent: IView = raise (System.NotImplementedException())
        member this.Remove(item: IView) = failwith "todo"
        member this.RemoveAt(index: int) = failwith "todo"
        member this.Rotation = failwith "todo"
        member this.RotationX = failwith "todo"
        member this.RotationY = failwith "todo"
        member this.Scale = failwith "todo"
        member this.ScaleX = failwith "todo"
        member this.ScaleY = failwith "todo"
        member this.Semantics = failwith "todo"
        member x.Spacing = A.StackLayout.Spacing.Get x.Attributes
        member this.TranslationX = failwith "todo"
        member this.TranslationY = failwith "todo"
        member this.VerticalLayoutAlignment = failwith "todo"
        member this.Visibility = failwith "todo"
        member this.Width = failwith "todo"

module LayoutThings =
    let doArrange (view: IView, bounds: Rectangle) : Size =
        let frame =
            LayoutExtensions.ComputeFrame(view, bounds)

        view.Frame <- frame

        match view.Handler with
        | null -> ()
        | handler -> handler.NativeArrange(frame)

        frame.Size

//    let measure (view: IView, widthConstraint: double, heightConstraint: double) : Size =




type FabulousLabel() =
    member val Attributes: Attribute [] = [||]
    member val Bounds: Rectangle = Rectangle.Zero with get, set
    member val ParentEl: IElement = null with get, set
    member val ActualDesiredSize: Size = Size.Zero with get, set


    interface IElement with
        member val Handler: IElementHandler = null with get, set
        member x.Parent: IElement = x.ParentEl

    interface IView with
        member x.Opacity = A.View.Opacity.Get x.Attributes
        member x.Margin = A.View.Margin.Get x.Attributes
        member x.AutomationId = A.View.AutomationId.Get x.Attributes
        member x.FlowDirection = A.View.FlowDirection.Get x.Attributes
        member x.Background = A.View.BackgroundColor.Get x.Attributes

        member x.Parent: IView = x.ParentEl :?> IView

        member this.Frame
            with get (): Rectangle = this.Bounds
            and set (v: Rectangle): unit = this.Bounds <- v

        member this.Handler
            with get (): IViewHandler = (this :> IElement).Handler :?> IViewHandler
            and set (v: IViewHandler): unit = (this :> IElement).Handler <- v

        member x.IsEnabled = A.View.IsEnabled.Get x.Attributes
        member x.Visibility = A.View.IsVisible.Get x.Attributes

        member x.HorizontalLayoutAlignment =
            A.View.HorizontalLayoutAlignment.Get x.Attributes

        member x.VerticalLayoutAlignment =
            A.View.VerticalLayoutAlignment.Get x.Attributes

        member this.DesiredSize = this.ActualDesiredSize
        member this.Arrange(bounds: Rectangle) = LayoutThings.doArrange (this, bounds)

        member this.Measure(widthConstraint: float, heightConstraint: float) =
            let size =
                LayoutExtensions.ComputeDesiredSize(this, widthConstraint, heightConstraint)

            this.ActualDesiredSize <- size
            size

        member this.InvalidateArrange() = failwith "todo"
        member this.InvalidateMeasure() = failwith "todo"
        member this.Height = 0. // TODO
        member this.Clip = null // TODO
        member this.Width = 0. // TODO
        member this.Semantics = null // TODO

    interface IPadding with
        member x.Padding = A.Padding.Padding.Get x.Attributes

    interface ITransform with
        member x.AnchorX = A.Transform.AnchorX.Get x.Attributes
        member x.AnchorY = A.Transform.AnchorY.Get x.Attributes
        member x.Rotation = A.Transform.Rotation.Get x.Attributes
        member x.RotationX = A.Transform.RotationX.Get x.Attributes
        member x.RotationY = A.Transform.RotationY.Get x.Attributes
        member x.Scale = A.Transform.Scale.Get x.Attributes
        member x.ScaleX = A.Transform.ScaleX.Get x.Attributes
        member x.ScaleY = A.Transform.ScaleY.Get x.Attributes

        member x.TranslationX =
            A.Transform.TranslationX.Get x.Attributes

        member x.TranslationY =
            A.Transform.TranslationY.Get x.Attributes

    interface ITextAlignment with
        member x.HorizontalTextAlignment =
            A.TextAlignment.HorizontalTextAlignment.Get x.Attributes

        member x.VerticalTextAlignment =
            A.TextAlignment.VerticalTextAlignment.Get x.Attributes

    interface ITextStyle with
        member x.TextColor = A.TextStyle.TextColor.Get x.Attributes
        member x.Font = A.TextStyle.Font.Get x.Attributes

        member x.CharacterSpacing =
            A.TextStyle.CharacterSpacing.Get x.Attributes

    interface IText with
        member x.Text = A.Text.Text.Get x.Attributes

    interface ILabel with
        member x.LineBreakMode = A.Label.LineBreakMode.Get x.Attributes
        member x.LineHeight = A.Label.LineHeight.Get x.Attributes
        member x.TextDecorations = A.Label.TextDecorations.Get x.Attributes
        member x.MaxLines = A.Label.MaxLines.Get x.Attributes


type FabulousButton() =
    member val Attributes: Attribute [] = [||]
    member val Dispatch: (obj -> unit) = fun (_) -> ()

    interface IButton with
        member this.Clicked() =
            match A.Button.Clicked.Get this.Attributes with
            | None -> ()
            | Some msg -> this.Dispatch(msg)

        member this.Pressed() = failwith "todo"
        member this.Released() = failwith "todo"

    interface IView with
        member x.Opacity = A.View.Opacity.Get x.Attributes
        member x.Margin = A.View.Margin.Get x.Attributes
        member this.Arrange(bounds: Rectangle) = failwith "todo"
        member this.AutomationId = failwith "todo"
        member this.Background = failwith "todo"
        member this.Clip = failwith "todo"
        member this.DesiredSize = failwith "todo"
        member this.FlowDirection = failwith "todo"

        member this.Frame
            with get (): Rectangle = raise (System.NotImplementedException())
            and set (v: Rectangle): unit = raise (System.NotImplementedException())

        member this.Handler
            with get (): IViewHandler = raise (System.NotImplementedException())
            and set (v: IViewHandler): unit = raise (System.NotImplementedException())

        member this.Height = failwith "todo"
        member this.HorizontalLayoutAlignment = failwith "todo"

        member this.InvalidateArrange() = failwith "todo"
        member this.InvalidateMeasure() = failwith "todo"
        member this.IsEnabled = failwith "todo"

        member this.Measure(widthConstraint: float, heightConstraint: float) = failwith "todo"

        member this.Parent: IView = raise (System.NotImplementedException())

        member this.Semantics = failwith "todo"

        member this.VerticalLayoutAlignment = failwith "todo"

        member this.Visibility = failwith "todo"
        member this.Width = failwith "todo"

    interface IElement with
        member val Handler: IElementHandler = null with get, set
        member this.Parent: IElement = raise (System.NotImplementedException())

    interface IText with
        member x.Text = A.Text.Text.Get x.Attributes

    interface IPadding with
        member x.Padding = A.Padding.Padding.Get x.Attributes

    interface ITextStyle with
        member x.TextColor = A.TextStyle.TextColor.Get x.Attributes
        member x.Font = A.TextStyle.Font.Get x.Attributes

        member x.CharacterSpacing =
            A.TextStyle.CharacterSpacing.Get x.Attributes

    interface ITransform with
        member x.AnchorX = A.Transform.AnchorX.Get x.Attributes
        member x.AnchorY = A.Transform.AnchorY.Get x.Attributes
        member x.Rotation = A.Transform.Rotation.Get x.Attributes
        member x.RotationX = A.Transform.RotationX.Get x.Attributes
        member x.RotationY = A.Transform.RotationY.Get x.Attributes
        member x.Scale = A.Transform.Scale.Get x.Attributes
        member x.ScaleX = A.Transform.ScaleX.Get x.Attributes
        member x.ScaleY = A.Transform.ScaleY.Get x.Attributes

        member x.TranslationX =
            A.Transform.TranslationX.Get x.Attributes

        member x.TranslationY =
            A.Transform.TranslationY.Get x.Attributes



// WIDGETS

[<Struct>]
type ApplicationWidget(attributes: Attribute []) =
    static do register<ApplicationWidget, FabulousApplication> ()

    static member inline Create(windows: seq<#IWindowWidget>) =
        ApplicationWidget(
            [|
                A.Application.Windows.WithValue(windows |> Seq.map (fun w -> w :> IWindowWidget))
            |]
        )

    interface IApplicationWidget

    interface IControlWidget with
        member this.Add(attribute) =
            addAttribute ApplicationWidget attributes attribute

        member this.CreateView() = box (FabulousApplication())

[<Struct>]
type WindowWidget(attributes: Attribute []) =
    static do register<WindowWidget, FabulousWindow> ()

    static member inline Create(title: string, content: IViewWidget) =
        WindowWidget(
            [|
                A.Window.Title.WithValue(title)
                A.Window.Content.WithValue(Some content)
            |]
        )

    interface IWindowWidget

    interface IControlWidget with
        member this.Add(attribute) =
            addAttribute WindowWidget attributes attribute

        member this.CreateView() = Unchecked.defaultof<_>

[<Struct>]
type StackLayoutWidget(attributes: Attribute []) =
    static do register<StackLayoutWidget, FabulousStackLayout> ()

    static member inline Create(children: seq<IViewWidget>) =
        StackLayoutWidget(
            [|
                A.Container.Children.WithValue(children)
            |]
        )

    interface IViewWidget

    interface IControlWidget with
        member this.Add(attribute) =
            addAttribute StackLayoutWidget attributes attribute

        member this.CreateView() = box (FabulousWindow())

[<Struct>]
type LabelWidget(attributes: Attribute []) =
    static do register<LabelWidget, FabulousLabel> ()

    static member inline Create(text: string) =
        LabelWidget(
            [|
                Attributes.Text.Text.WithValue(text)
            |]
        )

    interface IViewWidget

    interface IControlWidget with
        member this.Add(attribute) =
            addAttribute LabelWidget attributes attribute

        member this.CreateView() = box (FabulousLabel())

[<Struct>]
type ButtonWidget =
    {
        Attributes: Attribute []
    }
    interface IViewWidget

    interface IControlWidget with
        member this.Add(attribute) =
            addAttribute (fun attrs -> { Attributes = attrs }) this.Attributes attribute

        member this.CreateView() = box (FabulousButton())

    // TODO add typesafety to Widgets in general
    // probably they needs to be parametrized by Msg
    // in particular obj below should be replaced by Msg
    static member inline Create(text: string, clicked: (#obj)) =
        register<ButtonWidget, FabulousButton> ()

        {
            Attributes =
                [|
                    A.Text.Text.WithValue(text)
                    A.Button.Clicked.WithValue(Some (box clicked))
                |]
        }

// EXTENSIONS

[<Extension>]
type IViewWidgetExtensions() =
    [<Extension>]
    static member inline font<'T when 'T :> IViewWidget and 'T :> IControlWidget>(this: 'T, value: Font) =
        this.AddAttribute(Attributes.TextStyle.Font.WithValue(value))

[<Extension>]
type LabelWidgetExtensions() =
    [<Extension>]
    static member inline textColor(this: LabelWidget, value: Color) =
        this.AddAttribute(Attributes.TextStyle.TextColor.WithValue(value))

[<Extension>]
type StackLayoutExtensions() =
    [<Extension>]
    static member inline spacing(this: StackLayoutWidget, value: float) =
        this.AddAttribute(Attributes.StackLayout.Spacing.WithValue(value))

// EXPOSITION

[<AbstractClass; Sealed>]
type View private () =
    static member inline Application(windows) = ApplicationWidget.Create(windows)
    static member inline Window(title, content) = WindowWidget.Create(title, content)
    static member inline StackLayout(children) = StackLayoutWidget.Create(children)
    static member inline Label(text) = LabelWidget.Create(text)
    static member inline Button(text, clicked) = ButtonWidget.Create(text, clicked)
