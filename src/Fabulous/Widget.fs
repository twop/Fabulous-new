namespace Fabulous


open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Struct>]
type RunnerKey = RunnerKey of int

[<Struct>]
type AttributeKey = AttributeKey of int

[<Struct>]
type Attribute = {
    Key: AttributeKey
    Value: obj
    #if DEBUG
    Name: string
    #endif
}

module Attributes =
    [<Struct>]
    [<RequireQualifiedAccess>]
    type AttributeComparison =
        | Same
        | Different of data: obj option
        
    type IAttributeDefinition =
        abstract CompareBoxed: obj * obj -> AttributeComparison


    type AttributeComparer<'modelType> = 'modelType -> 'modelType -> AttributeComparison

    type AttributeDefinition<'inputType, 'modelType> =
        {
            Key: AttributeKey
            Name: string
            DefaultValue: 'modelType
            Convert: 'inputType -> 'modelType
            Compare: AttributeComparer<'modelType>
        }
        interface IAttributeDefinition with
            member this.CompareBoxed(a, b) = this.Compare (unbox<'modelType>a) (unbox<'modelType>b) 

        member x.WithValue(value): Attribute =
            {
                Key = x.Key
                Value = x.Convert(value)
                #if DEBUG
                Name = x.Name
                #endif
            }

        member x.TryGet(attributes: Attribute []) =
            let attr =
                attributes
                |> Array.tryFind(fun attr -> attr.Key = x.Key)

            match attr with
            | Some attr -> Some(unbox<'modelType> attr.Value)
            | None -> None

        member x.Get(attributes: Attribute []) =
            x.TryGet attributes
            |> Option.defaultValue x.DefaultValue



    let private _attributes =
        Dictionary<AttributeKey, IAttributeDefinition>()

    let createDefinitionWithConverter<'inputType, 'modelType>
        (name: string)
        (defaultValue: 'modelType)
        (convert: 'inputType -> 'modelType)
        (compare: 'modelType -> 'modelType -> AttributeComparison)
        : AttributeDefinition<'inputType, 'modelType> =

        let key = AttributeKey(_attributes.Count + 1)

        let definition =
            {
                Key = key
                Convert = convert
                Name = name
                DefaultValue = defaultValue
                Compare = compare
            }

        _attributes.Add(key, definition)
        definition

    let createDefinition<'T> comparer name defaultValue =
        createDefinitionWithConverter<'T, 'T> name defaultValue id comparer

    let inline define<'T when 'T: equality> name defaultValue =
        createDefinition<'T>
            (fun a b ->
                if a = b then
                    AttributeComparison.Same
                else
                    (AttributeComparison.Different None))
            name
            defaultValue
            
    /// This is insertion sort that is O(n*n) but it performs better
    /// 1. if the array is partially sorted (second sort is cheap)
    /// 2. there are few elements, we expect to have only a handful of them per widget
    /// 3. stable, which is handy for duplicate attributes, e.g. we can choose which one to pick
    /// https://en.wikipedia.org/wiki/Insertion_sort
    let inline private sortAttributesInPlace (attrs: Attribute[]): Attribute [] =
        let N = attrs.GetLength(0)
        for i in [1..N-1] do
            for j = i downto 1 do
                if attrs.[j].Key < attrs.[j - 1].Key then
                    let temp = attrs.[j]
                    attrs.[j] <- attrs.[j - 1]
                    attrs.[j - 1] <- temp
        attrs

    [<RequireQualifiedAccess>]
    type AttributeDiff =
        | Different of struct (Attribute * Attribute * obj option)
        | Added of Attribute
        | Removed of Attribute

    /// Let's imagine that we have the following situation
    /// prev = [|1,2,6,7|] note that it is sorted
    /// next = [|8,5,6,2|] unsorted
    /// In reality we have Key and Value, but let's pretend that we only care about keys for now 
    ///
    /// then the desired outcome is this
    /// added = [5, 8]
    /// removed = [1, 7]
    ///
    /// Approach
    /// 1. we sort both arrays
    /// prev = [|1,2,6,7|] 
    /// next = [|2,5,6,8|]
    ///
    /// 2. Starting from index 0 for both of the arrays
    ///     if prevItem < nextItem then
    ///         inc prevIndex, add prevItem to removed, goto 2
    ///     if prevItem > nextItem 
    ///         inc nextIndex, add nextItem to added, goto 2
    ///     else (meaning equals)
    ///         compare values
    ///
    /// break when we reached both ends of the arrays
    let compareAttributes (prev: Attribute []) (next: Attribute []) : AttributeDiff list =
        // the order of attributes doesn't matter, thus it is safe to mutate array in place
        prev |> sortAttributesInPlace |> ignore
        next |> sortAttributesInPlace |> ignore

        let mutable result: AttributeDiff list = []
        
        let mutable prevIndex = 0
        let mutable nextIndex = 0
        
        let prevLength = prev.Length
        let nextLength = next.Length
        
        while not (prevIndex >= prevLength && nextIndex >= nextLength) 
            do
                if prevIndex = prevLength then
                    // that means we are done with the prev and only need to add next's tail to added
                    result <- AttributeDiff.Added next.[nextIndex] :: result
                    nextIndex <- nextIndex + 1
                    
                elif nextIndex = nextLength then
                    // that means that we are done with new items and only need prev's tail to removed
                    result <- AttributeDiff.Removed prev.[prevIndex] :: result
                    prevIndex <- prevIndex + 1
                    
                else
                    // we haven't reached either of the ends 
                    let prevItem = prev.[prevIndex]
                    let nextItem = next.[nextIndex]
                    
                    let (AttributeKey prevKey) = prevItem.Key
                    let (AttributeKey nextKey) = nextItem.Key
                    
                    match prevKey.CompareTo nextKey with
                    | c when c < 0 ->
                        // prev key is less than next -> remove prev key
                        result <- AttributeDiff.Removed prevItem :: result
                        prevIndex <- prevIndex + 1
                        
                    | c when c > 0 ->
                        // prev key is more than next -> add next item
                        result <- AttributeDiff.Added nextItem :: result
                        nextIndex <- nextIndex + 1
                        
                    | _ ->
                        // means that we are targeting the same attribute
                        
                        // move both pointers
                        prevIndex <- prevIndex + 1
                        nextIndex <- nextIndex + 1
                    
                        let attribute = _attributes.[prevItem.Key]
                        match attribute.CompareBoxed (prevItem.Value, nextItem.Value) with
                        | AttributeComparison.Same -> ()
                        | AttributeComparison.Different diff ->
                            result <- AttributeDiff.Different (prevItem, nextItem, diff) :: result
                        
        result


/// Base logical element
type IWidget =
    abstract CreateView : unit -> IViewNode
    abstract Attributes : Attribute []
    
and ITypedWidget<'msg> =
    inherit IWidget

and IViewNode =
    // is this needed?
    // maybe have something like WidgetType and Attributes separately?
    abstract Attributes : Attribute []
    abstract Source : Type
    abstract ApplyDiff : (Attributes.AttributeDiff list * Attribute []) -> UpdateResult

and [<Struct>] ChildrenUpdate =
    {
        Children: IViewNode []
        Added: IViewNode list option
        Removed: IViewNode list option
    }

// TODO should it be IList? or a simpler custom interface will do?
and IViewContainer =
    abstract Children : IViewNode []
    abstract UpdateChildren : ChildrenUpdate -> unit

and [<RequireQualifiedAccess; Struct>] UpdateResult =
    | Done
    | UpdateChildren of struct (IViewContainer * IWidget [])


module ControlWidget =
    type IControlWidget =
        inherit IWidget
        abstract Add : Attribute -> IControlWidget

    type Handler =
        {
            TargetType: Type
            Create: Attribute [] -> obj
        }

    let private _handlers = Dictionary<Type, Handler>()

    let registerWithCustomCtor<'Builder, 'T> (create: Attribute [] -> 'T) =
        if not(_handlers.ContainsKey(typeof<'Builder>)) then
            _handlers.[typeof<'Builder>] <-
                {
                    TargetType = typeof<'T>
                    Create = create >> box
                }

    let register<'Builder, 'T when 'T: (new : unit -> 'T)> () =
        registerWithCustomCtor<'Builder, 'T>(fun _ -> new 'T())


    let inline addAttribute (fn: Attribute [] -> #IControlWidget) (attribs: Attribute []) (attr: Attribute) =
        let attribs2 = Array.zeroCreate(attribs.Length + 1)
        Array.blit attribs 0 attribs2 0 attribs.Length
        //        printfn "%A %A" attribs2.Length attribs.Length
        attribs2.[attribs.Length] <- attr
        (fn attribs2) :> IControlWidget

    [<Extension>]
    type IControlWidgetExtensions() =
        [<Extension>]
        static member inline AddAttribute<'T when 'T :> IControlWidget>(this: 'T, attr: Attribute) =
            this.Add(attr) :?> 'T



/// Logical element without state able to generate a logical tree composed of child widgets
type IStatelessWidget<'view when 'view :> IWidget> =
    abstract View : Attribute [] -> 'view

/// Logical element with MVU state able to generate a logical tree composed of child widgets
type IStatefulWidget<'arg, 'model, 'msg, 'view when 'view :> IWidget> =
    abstract State : RunnerKey option
    abstract Init : 'arg -> 'model
    abstract Update : 'msg * 'model -> 'model
    abstract View : 'model -> 'view


//-----Update sketch------
module Reconciler =

    // https://medium.com/@deathmood/how-to-write-your-own-virtual-dom-ee74acc13060
//    function updateElement($parent, newNode, oldNode, index = 0) {
//  if (!oldNode) {
//    $parent.appendChild(
//      createElement(newNode)
//    );
//  } else if (!newNode) {
//    $parent.removeChild(
//      $parent.childNodes[index]
//    );
//  } else if (changed(newNode, oldNode)) {
//    $parent.replaceChild(
//      createElement(newNode),
//      $parent.childNodes[index]
//    );
//  } else if (newNode.type) {
//    const newLength = newNode.children.length;
//    const oldLength = oldNode.children.length;
//    for (let i = 0; i < newLength || i < oldLength; i++) {
//      updateElement(
//        $parent.childNodes[index],
//        newNode.children[i],
//        oldNode.children[i],
//        i
//      );
//    }
//  }
//}

    let inline private addItem item maybeList =
        match maybeList with
        | Some l -> Some(item :: l)
        | None -> Some [ item ]

    //    let private isSame
    let rec update (node: IViewNode) (attributes: Attribute []) : unit =
        let diff =
            Attributes.compareAttributes node.Attributes attributes

        if List.isEmpty diff then
            ()
        else
            match node.ApplyDiff(diff, attributes) with
            | UpdateResult.Done -> ()
            | UpdateResult.UpdateChildren struct (container, widgets) ->
                let children = container.Children

                // if the size is the same we can just reuse the same array to avoid allocations
                // it is safe to do so because diffing goes only forward, thus safe to do it in place
                let target: IViewNode [] =
                    if widgets.Length = children.Length then
                        children
                    else
                        Array.zeroCreate(widgets.Length)

                let mutable added: IViewNode list option = None

                let mutable removed: IViewNode list option =
                    // if we are downsizing then the tail needs to be added to removed
                    if children.Length > widgets.Length then
                        children
                        |> Array.skip widgets.Length
                        |> Array.toList
                        |> Some
                    else
                        None

                for i = 0 to widgets.Length - 1 do
                    let widget = widgets.[i]
                    let prev = Array.tryItem i children

                    match (prev, widget) with
                    | None, widget ->
                        // view doesn't exist yet
                        let view = widget.CreateView()
                        target.[i] <- view
                        added <- addItem view added

                    | Some p, widget when widget.GetType() = p.Source ->
                        // same type, just update
                        target.[i] <- p
                        update p widget.Attributes

                    | Some p, widget ->
                        // different type, thus replacement is needed
                        let view = widget.CreateView()
                        target.[i] <- view
                        added <- addItem view added
                        removed <- addItem p removed

                container.UpdateChildren
                    {
                        Children = target
                        Added = added
                        Removed = removed
                    }

                ()



// 1. compare attributes for control and widget
// 2. apply diff (should be a method on control). e.g control.ApplyDiff(diff)
// 3. apply returns UpdateResult
// 4. if we have 'Done' then exit
// 5. if we have 'UpdateNext' go through the list and recursively call update on each element

// Questions
// 1. should adding/removing children should be done here as well?
// 2. Should 'mounting' views done by core framework or should be part of MAUI? I think the latter is fine
// 3. Should widgets have 1st class parent -> child relationships like in react?
// Meaning that we can fully control creations of new controls via core
