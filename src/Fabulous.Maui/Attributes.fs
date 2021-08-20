module Fabulous.Maui.Attributes

open Fabulous
open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Primitives


module Container =
    let Children =
        Attributes.createDefinitionWithConverter<seq<IWidget>, IWidget []>
            "Children"
            [||]
            Seq.toArray
            (fun a b -> Attributes.AttributeComparison.NotSure)

module Window =
    let Title = Attributes.define<string> "Title" ""

    let Content =
        Attributes.createDefinition<IViewWidget option>
            (fun a b -> Attributes.AttributeComparison.NotSure)
            "Content"
            None // TODO remove option

module Application =
    let Windows =
        Attributes.createDefinitionWithConverter<seq<IWindowWidget>, IWindowWidget []>
            "Windows"
            [||]
            Seq.toArray
            (fun a b -> Attributes.AttributeComparison.NotSure)


module View =
    let Margin =
        Attributes.define<Thickness> "Margin" Thickness.Zero

    let Opacity = Attributes.define<float> "Opacity" 1.0

    let AutomationId =
        Attributes.define<string> "AutomationId" ""

    let FlowDirection =
        Attributes.define<Microsoft.Maui.FlowDirection>

            "FlowDirection"
            Microsoft.Maui.FlowDirection.LeftToRight // TODO default value

    let HorizontalLayoutAlignment =
        Attributes.define<LayoutAlignment> "HorizontalLayoutAlignment" LayoutAlignment.Start

    let VerticalLayoutAlignment =
        Attributes.define<LayoutAlignment> "VerticalLayoutAlignment" LayoutAlignment.Start

    let IsEnabled = Attributes.define<bool> "IsEnabled" true

    let isEqual a b =
        if a = b then
            Attributes.AttributeComparison.Same
        else
            (Attributes.AttributeComparison.Different None)

    // Note that View actually has a more complicated type but we support only coloring backs for now
    let BackgroundColor =
        Attributes.createDefinitionWithConverter<Color, Paint>
            "Background"
            null
            (fun color -> (SolidPaint color) :> Paint)
            isEqual


    let IsVisible =
        Attributes.createDefinitionWithConverter<bool, Visibility>
            "IsVisible"
            Visibility.Visible
            VisibilityExtensions.ToVisibility
            isEqual


module StackLayout =
    let Spacing = Attributes.define<float> "Spacing" 0.0

module TextStyle =
    let TextColor =
        Attributes.define<Color> "TextColor" null

    // default value from MAUI code
    // Font.OfSize(element.FontFamily, element.FontSize, enableScaling: element.FontAutoScalingEnabled).WithAttributes(element.FontAttributes);
    let Font =
        Attributes.define<Font> "Font" Font.Default //TODO

    let CharacterSpacing =
        Attributes.define<float> "CharacterSpacing" 0.0

module Text =
    let Text = Attributes.define<string> "Text" ""

module Label =
    let LineBreakMode =
        Attributes.define<LineBreakMode> "LineBreakMode" LineBreakMode.WordWrap

    let LineHeight =
        Attributes.define<float> "LineHeight" -1.0 // TODO why -1?

    let TextDecorations =
        Attributes.define<TextDecorations> "TextDecorations" TextDecorations.None

    let MaxLines = Attributes.define<int> "MaxLines" -1

module Padding =
    let Padding =
        Attributes.define<Thickness> "Padding" Thickness.Zero


module Transform =
    let AnchorX = Attributes.define<float> "AnchorX" 0.5
    let AnchorY = Attributes.define<float> "AnchorY" 0.5

    let TranslationX =
        Attributes.define<float> "TranslationX" 0.0

    let TranslationY =
        Attributes.define<float> "TranslationY" 0.0

    let Rotation = Attributes.define<float> "Rotation" 0.0
    let RotationX = Attributes.define<float> "RotationX" 0.0
    let RotationY = Attributes.define<float> "RotationY" 0.0
    let Scale = Attributes.define<float> "Scale" 1.0
    let ScaleX = Attributes.define<float> "ScaleX" 1.0
    let ScaleY = Attributes.define<float> "ScaleY" 1.0


module TextAlignment =
    let HorizontalTextAlignment =
        Attributes.define<TextAlignment> "HorizontalTextAlignment" TextAlignment.Start

    let VerticalTextAlignment =
        Attributes.define<TextAlignment> "VerticalTextAlignment" TextAlignment.Center

module Button =
    let Clicked =
        // TODO what is the proper way to represent that?
        Attributes.createDefinition<obj option> (fun a b -> Attributes.AttributeComparison.NotSure) "_Clicked" None // TODO fix option
