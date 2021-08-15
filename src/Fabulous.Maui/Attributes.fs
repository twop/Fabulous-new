module Fabulous.Maui.Attributes

open Fabulous
open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Primitives


module Container =
    let Children =
        Attributes.createDefinitionWithConverter<seq<IViewWidget>, IViewWidget []> "Children" [||] Seq.toArray

module Window =
    let Title =
        Attributes.createDefinition<string> "Title" ""

    let Content =
        Attributes.createDefinition<IViewWidget option> "Content" None // TODO remove option

module Application =
    let Windows =
        Attributes.createDefinitionWithConverter<seq<IWindowWidget>, IWindowWidget []> "Windows" [||] Seq.toArray


module View =
    let Margin =
        Attributes.createDefinition<Thickness> "Margin" Thickness.Zero

    let Opacity =
        Attributes.createDefinition<float> "Opacity" 1.0

    let AutomationId =
        Attributes.createDefinition<string> "AutomationId" ""

    let FlowDirection =
        Attributes.createDefinition<Microsoft.Maui.FlowDirection>
            "FlowDirection"
            Microsoft.Maui.FlowDirection.LeftToRight // TODO default value

    let HorizontalLayoutAlignment =
        Attributes.createDefinition<LayoutAlignment> "HorizontalLayoutAlignment" LayoutAlignment.Start

    let VerticalLayoutAlignment =
        Attributes.createDefinition<LayoutAlignment> "VerticalLayoutAlignment" LayoutAlignment.Start

    let IsEnabled =
        Attributes.createDefinition<bool> "IsEnabled" true

    // Note that View actually has a more complicated type but we support only coloring backs for now
    let BackgroundColor =
        Attributes.createDefinitionWithConverter<Color, Paint>
            "Background"
            null
            (fun color -> (SolidPaint color) :> Paint)

    let IsVisible =
        Attributes.createDefinitionWithConverter<bool, Visibility>
            "IsVisible"
            Visibility.Visible
            VisibilityExtensions.ToVisibility


module StackLayout =
    let Spacing =
        Attributes.createDefinition<float> "Spacing" 0.0

module TextStyle =
    let TextColor =
        Attributes.createDefinition<Color> "TextColor" null

    // default value from MAUI code
    // Font.OfSize(element.FontFamily, element.FontSize, enableScaling: element.FontAutoScalingEnabled).WithAttributes(element.FontAttributes);
    let Font =
        Attributes.createDefinition<Font> "Font" Font.Default //TODO

    let CharacterSpacing =
        Attributes.createDefinition<float> "CharacterSpacing" 0.0

module Text =
    let Text =
        Attributes.createDefinition<string> "Text" ""

module Label =
    let LineBreakMode =
        Attributes.createDefinition<LineBreakMode> "LineBreakMode" LineBreakMode.WordWrap

    let LineHeight =
        Attributes.createDefinition<float> "LineHeight" -1.0 // TODO why -1?

    let TextDecorations =
        Attributes.createDefinition<TextDecorations> "TextDecorations" TextDecorations.None

    let MaxLines =
        Attributes.createDefinition<int> "MaxLines" -1

module Padding =
    let Padding =
        Attributes.createDefinition<Thickness> "Padding" Thickness.Zero


module Transform =
    let AnchorX =
        Attributes.createDefinition<float> "AnchorX" 0.5

    let AnchorY =
        Attributes.createDefinition<float> "AnchorY" 0.5

    let TranslationX =
        Attributes.createDefinition<float> "TranslationX" 0.0

    let TranslationY =
        Attributes.createDefinition<float> "TranslationY" 0.0

    let Rotation =
        Attributes.createDefinition<float> "Rotation" 0.0

    let RotationX =
        Attributes.createDefinition<float> "RotationX" 0.0

    let RotationY =
        Attributes.createDefinition<float> "RotationY" 0.0

    let Scale =
        Attributes.createDefinition<float> "Scale" 1.0

    let ScaleX =
        Attributes.createDefinition<float> "ScaleX" 1.0

    let ScaleY =
        Attributes.createDefinition<float> "ScaleY" 1.0


module TextAlignment =
    let HorizontalTextAlignment =
        Attributes.createDefinition<TextAlignment> "HorizontalTextAlignment" TextAlignment.Start

    let VerticalTextAlignment =
        Attributes.createDefinition<TextAlignment> "VerticalTextAlignment" TextAlignment.Center

module Button =
    let Clicked =
        Attributes.createDefinition<(unit -> obj) option> "_Clicked" None
