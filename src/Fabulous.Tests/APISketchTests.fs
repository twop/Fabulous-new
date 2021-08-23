module Fabulous.Tests

open Fabulous.Reconciler
open NUnit.Framework

open type TestUI.View
open TestUI


//System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName


module SimpleLabelTests =
    type Msg =
        | SetText of string
        | SetColor of string

    type Model = { text: string; color: string }

    let update msg model =
        match msg with
        | SetText text -> { model with text = text }
        | SetColor color -> { model with color = color }

    let view model =
        Label(model.text)
            .textColor(model.color)
            .automationId("label")

    let init () = { text = "hi"; color = "red" }

    [<Test>]
    let SketchAPI () =
        let program =
            StatefulWidget.mkSimpleView init update view

        let instance = Run.Instance program

        let tree = (instance.Start())

        let label =
            tree.FindByAutomationId "label"
            |> Option.defaultWith(fun _ -> failwith "not found")
            :?> TestLabel

        Assert.AreEqual(label.Text, "hi")
        instance.ProcessMessage(SetText "yo")
        Assert.AreEqual(label.Text, "yo")

        Assert.AreEqual(label.Color, "red")
        instance.ProcessMessage(SetColor "blue")
        Assert.AreEqual(label.Color, "blue")


module ButtonTests =
    type Msg = | Increment

    type Model = { count: int }

    let update msg model =
        match msg with
        | Increment -> { model with count = model.count + 1 }

    let view model =
        Button(model.count.ToString(), Increment)
            .automationId("btn")

    let init () = { count = 0 }

    [<Test>]
    let SketchAPI () =
        let program =
            StatefulWidget.mkSimpleView init update view

        let instance = Run.Instance program
        let tree = (instance.Start())
        let btn =
            tree.FindByAutomationId "btn"
            |> Option.defaultWith(fun _ -> failwith "not found")
            :?> TestButton

        Assert.AreEqual(btn.Text, "0")
        btn.Press()
        Assert.AreEqual(btn.Text, "1")
