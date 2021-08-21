module Fabulous.Tests

open Fabulous.Reconciler
open NUnit.Framework

open type TestUI.View
open TestUI


//System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName


module SimpleProgram =
    type Msg =
        | SetText of string
        | SetColor of string

    type Model = { text: string; color: string }

    let update msg model =
        match msg with
        | SetText text -> { model with text = text }
        | SetColor color -> { model with color = color }

    let view model =
        Label(model.text).textColor(model.color)

    let init () = { text = "hi"; color = "red" }

    [<Test>]
    let SketchAPI () =
        let program =
            StatefulWidget.mkSimpleView init update view

        let instance = Run.Instance program

        let viewNode = (instance.Start()) :?> TestLabel

        Assert.AreEqual(viewNode.Text, "hi")
        instance.ProcessMessage(SetText "yo")
        Assert.AreEqual(viewNode.Text, "yo")

        Assert.AreEqual(viewNode.Color, "red")
        instance.ProcessMessage(SetColor "blue")
        Assert.AreEqual(viewNode.Color, "blue")
