module Fabulous.Tests

open Fabulous.Reconciler
open NUnit.Framework

open type TestUI.View
open TestUI


//System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName

[<Test>]
let SketchAPI () =
    let labelWidget = Label("hi").textColor ("green")

    let viewNode =
        (labelWidget :> IWidget).CreateView() :?> TestLabel

    let update = Label("yo").textColor ("red")

    (viewNode :> IViewNode)
        .ApplyDiff(update :> IWidget, [])
    |> ignore

    Assert.AreEqual(viewNode.Color, "red")
    Assert.AreEqual(viewNode.Text, "yo")
