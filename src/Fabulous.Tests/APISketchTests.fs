module Fabulous.Tests

open Fabulous.Reconciler
open NUnit.Framework

open type TestUI.View
open TestUI


//System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName

let find<'a when 'a :> IViewNode> (tree: Run.ViewTree) (id: string) =
    let node =
        tree.FindByAutomationId id
        |> Option.defaultWith(fun _ -> failwith "not found")
        :?> 'a

    node

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

        let label = find<TestLabel> tree "label"

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

        let btn = find<TestButton> tree "btn"

        Assert.AreEqual(btn.Text, "0")
        btn.Press()
        Assert.AreEqual(btn.Text, "1")


module SimpleStackTests =
    type Msg =
        | Delete of id: int
        | AddNew of id: int * text: string
        | ChangeText of id: int * text: string

    type Model = (int * string) list

    let update msg model =
        match msg with
        | Delete id -> model |> List.filter(fun (id_, _) -> id_ <> id)
        | AddNew (id, text) -> (id, text) :: model
        | ChangeText (id, text) ->
            model
            |> List.map
                (fun (id_, text_) ->
                    if id = id_ then
                        (id, text)
                    else
                        (id_, text_))

    let view model =
        let labels =
            model
            |> List.map(fun (id, text) -> Label(text).automationId(id.ToString()))

        Stack(labels).automationId("stack")


    let init () = []

    [<Test>]
    let SketchAPI () =
        let program =
            StatefulWidget.mkSimpleView init update view

        let instance = Run.Instance program

        let tree = (instance.Start())

        let stack = find<TestStack> tree "stack" :> IViewContainer
        Assert.AreEqual(stack.Children.Length, 0)
        
        // add first
        instance.ProcessMessage(AddNew (1, "yo"))
        Assert.AreEqual(stack.Children.Length, 1)
        let label = stack.Children.[0] :?> TestLabel
        Assert.AreEqual(label.Text, "yo")
        
        // add second in front
        instance.ProcessMessage(AddNew (2, "yo2"))
        Assert.AreEqual(stack.Children.Length, 2)
        let label = stack.Children.[0] :?> TestLabel
        Assert.AreEqual(label.Text, "yo2")
        
        // modify the initial one
        instance.ProcessMessage(ChangeText (1, "just 1"))
        let label = stack.Children.[1] :?> TestLabel
        Assert.AreEqual(label.Text, "just 1")
        
        // delete the one in front
        instance.ProcessMessage(Delete 2)
        Assert.AreEqual(stack.Children.Length, 1)
        let label = stack.Children.[0] :?> TestLabel
        Assert.AreEqual(label.Text, "just 1")
        
        
