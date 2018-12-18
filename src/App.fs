module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL

type Item =
    { Title : string
      Description : string
      RewardPoints : string
      Deadline : string }

type TodoItem =
    | NewTodoItem of Item
    | AssignedTodoItem of Item * string
    | DoneTodoItem of Item * string
    | RemovedTodoItem of Item

type FormModel = 
    { Title : string
      Description : string 
      RewardPoints : string
      Deadline : string }

type Model =
    { TodoForm : FormModel
      TodoItems : TodoItem list
      AssigneeInput : string }

type Msg =
| UpdateFormTitle of string
| UpdateFormDescription of string
| UpdateFormRewardPoints of string
| UpdateFormDeadline of string
| CreateTodoItem
| UpdateAssigneeInput of string
| AssignTodoItem of string
| DeleteAssignment of string
| MarkDone of string
| RemoveItem of string

let init() : Model =
    { TodoForm = 
        { Title = ""
          Description = ""
          RewardPoints = ""
          Deadline = "" }
      TodoItems = [] 
      AssigneeInput = ""}

// UPDATE

let assign (title : string, assignee : string) (d : TodoItem) =
    match d with
    | NewTodoItem item ->
        if item.Title = title then
            sprintf "%s has been assigned to %s!" item.Title assignee
            |> Browser.console.log
            (AssignedTodoItem (item, assignee))
        else d
    | AssignedTodoItem (item, itemAssignee) ->
        if item.Title = title then
            if itemAssignee = assignee then
                sprintf "This todo-item (%s) is already assigned to the given assignee, %s" item.Title itemAssignee
                |> Browser.console.log
                d
            else 
                sprintf "Reassigned todo-item (%s) to %s (from %s)" item.Title assignee itemAssignee
                |> Browser.console.log
                AssignedTodoItem (item, assignee)
        else d
    | DoneTodoItem _ -> d
    | RemovedTodoItem _ -> d

let unassign (title : string) (d : TodoItem) =
    match d with
    | NewTodoItem _ -> d
    | AssignedTodoItem (item, assignee) -> 
        NewTodoItem item
    | DoneTodoItem (item, assignee) ->
        sprintf "item %s is already done, can't unassign.." item.Title
        |> Browser.console.log
        d
    | RemovedTodoItem _ -> d

let markDone (title : string) (d : TodoItem) =
    match d with
    | NewTodoItem item ->
        if item.Title = title then
            sprintf "item %s is unassigned, can't mark done without an assignment" item.Title
            |> Browser.console.log
            d
        else d
    | AssignedTodoItem (item, assignee) ->
        if item.Title = title then
            sprintf "marking item %s as done, was assigned to %s" item.Title assignee
            |> Browser.console.log
            DoneTodoItem (item, assignee)
        else d
    | DoneTodoItem (item, assignee) -> d
    | RemovedTodoItem _ -> d


let removeItem (title : string) (d : TodoItem) =
    match d with
    | NewTodoItem item ->
        if item.Title = title then
            sprintf "Removing item %s" item.Title
            |> Browser.console.log
            RemovedTodoItem (item)
        else d
    | AssignedTodoItem (item, _) ->
        if item.Title = title then
            sprintf "Removing item %s" item.Title
            |> Browser.console.log
            RemovedTodoItem (item)
        else d
    | DoneTodoItem (item, assignee) ->
        if item.Title = title then
            sprintf "Removing item %s" item.Title
            |> Browser.console.log
            RemovedTodoItem (item)
        else d
    | RemovedTodoItem _ -> d


let update (msg:Msg) (model:Model) =
    match msg with
    | UpdateFormTitle content ->
        { model with TodoForm = { Title = content
                                  Description = model.TodoForm.Description
                                  RewardPoints = model.TodoForm.RewardPoints
                                  Deadline = model.TodoForm.Deadline} }
    | UpdateFormDescription content ->
        { model with TodoForm = { Title = model.TodoForm.Title
                                  Description = content
                                  RewardPoints = model.TodoForm.RewardPoints
                                  Deadline = model.TodoForm.Deadline} }
    | UpdateFormRewardPoints content ->
        { model with TodoForm = { Title = model.TodoForm.Title
                                  Description = model.TodoForm.Description
                                  RewardPoints = content
                                  Deadline = model.TodoForm.Deadline} }
    | UpdateFormDeadline content ->
        { model with TodoForm = { Title = model.TodoForm.Title
                                  Description = model.TodoForm.Description
                                  RewardPoints = model.TodoForm.RewardPoints
                                  Deadline = content } }
    | CreateTodoItem ->
        let newItem: TodoItem = NewTodoItem { Title = model.TodoForm.Title 
                                              Description = model.TodoForm.Description 
                                              RewardPoints = model.TodoForm.RewardPoints
                                              Deadline = model.TodoForm.Deadline }
        //{ model with TodoForm = "empty" }
        { model with TodoForm = { Title = ""
                                  Description = ""
                                  RewardPoints = ""
                                  Deadline = ""}
                     TodoItems = newItem::model.TodoItems }
    | UpdateAssigneeInput input ->
        { model with AssigneeInput = input }
    | AssignTodoItem (title) ->
        let items =
            model.TodoItems
            |> List.map (assign (title, "You"))
        { model with TodoItems = items }
    | DeleteAssignment (title) ->
        let items =
            model.TodoItems
            |> List.map (unassign title)
        { model with TodoItems = items }
    | MarkDone (title) ->
        let items =
            model.TodoItems
            |> List.map (markDone (title))
        { model with TodoItems = items }
    | RemoveItem (title) ->
        let items =
            model.TodoItems
            |> List.map (removeItem (title))
        { model with TodoItems = items }



// VIEW (rendered with React)

open Fulma

let newTodoItem dispatch (item : Item) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str item.Title ] ]
              Card.content []
                [
                  Content.content [] [ str "Description:" ]
                  Content.content [] [ str item.Description ]
                  Content.content [] [ str "Currently unassigned"  ]
                  Content.content [] [ str "" ]
                  Content.content [] [ str "Reward:" ]
                  Content.content [] [ str item.RewardPoints ]
                  Content.content [] [ str "Deadline:" ]
                  Content.content [] [ str item.Deadline ]
                ]
              Card.footer []
                [
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> AssignTodoItem item.Title |> dispatch) ] ]
                    [ str "Assign to me" ] 
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveItem item.Title |> dispatch) ] ]
                    [ str "Remove" ] 
                ] 
            ] 
        ]

let doneTodoItem dispatch (item : Item) (assignee : string) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str item.Title ] ]
              Card.content []
                [ 
                  Content.content [] [ str "Description:" ]
                  Content.content [] [ str item.Description ]
                  Content.content [] [ sprintf "Done by %s" assignee |> str  ]
                  Content.content [] [ str "" ]
                  Content.content [] [ str "Reward:" ]
                  Content.content [] [ str item.RewardPoints ]
                  Content.content [] [ str "Deadline:" ]
                  Content.content [] [ str item.Deadline ]
                ]
              Card.footer []
                [
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveItem item.Title |> dispatch) ] ]
                    [ str "Remove" ] 
                ] 
            ] 
        ]

let assignedTodoItem dispatch (item : Item) (assignee : string) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str item.Title ] ]
              Card.content []
                [ 
                  Content.content [] [ str "Description:" ]
                  Content.content [] [ str item.Description ]
                  Content.content [] [ sprintf "Done by %s" assignee |> str  ]
                  Content.content [] [ str "" ]
                  Content.content [] [ str "Reward:" ]
                  Content.content [] [ str item.RewardPoints ]
                  Content.content [] [ str "Deadline:" ]
                  Content.content [] [ str item.Deadline ]
                ]
              Card.footer []
                [
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> DeleteAssignment item.Title |> dispatch) ] ]
                    [ str "Remove my assignment" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> MarkDone item.Title |> dispatch) ] ]
                    [ str "Mark as done" ]
                  Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> RemoveItem item.Title |> dispatch) ] ]
                    [ str "Remove" ] 
                ]
            ]
        ]

let removedTodoItem dispatch (item : Item) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str item.Title ] ]
              Card.content []
                [ 
                  Content.content [] [ str "This item has been removed" ]
                ]
              Card.footer []
                [
                ] 
            ] 
        ]

let toCard dispatch (draft : TodoItem) =
    match draft with
    | NewTodoItem item ->
        newTodoItem dispatch item
    | AssignedTodoItem (item, assignee) ->
        assignedTodoItem dispatch item assignee
    | DoneTodoItem (item, assignee) ->
        doneTodoItem dispatch item assignee
    | RemovedTodoItem item ->
        removedTodoItem dispatch item
        

let toCardRow row =
    Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ] row

let rec chunkByThree soFar l =
    match l with
    | x1::x2::[x3] ->
        [x1; x2; x3]::soFar
    | x1::x2::x3::xs ->
        chunkByThree ([x1; x2; x3]::soFar) xs
    | xs ->
        xs::soFar

let toCardRows dispatch (titles : TodoItem list) =
    titles
    |> chunkByThree []
    |> List.rev
    |> List.map ((List.map (toCard dispatch)) >> toCardRow)

let view (model:Model) dispatch =   
    div []
      [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ str "TodoApp" ] ] ]
        Container.container [ Container.IsFluid ]
          [ h1 [ Class "is-size-1 app-title" ] [ str "Manage your chores / TODOs" ]
            Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                [ yield Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ]
                    [ Tile.tile [ Tile.IsChild ]
                        [ Card.card []
                            [ Card.header []
                                [ Card.Header.title [] [ str "Add a new item!" ] ]
                              Card.content []
                                [ Input.text [ Input.Placeholder "Title"
                                               Input.Value model.TodoForm.Title
                                               Input.OnChange (fun ev -> UpdateFormTitle ev.Value |> dispatch) 
                                             ]
                                  Input.text [ Input.Placeholder "Description"
                                               Input.Value model.TodoForm.Description
                                               Input.OnChange (fun ev -> UpdateFormDescription ev.Value |> dispatch)
                                             ]
                                  Input.text [ Input.Placeholder "RewardPoints"
                                               Input.Value model.TodoForm.RewardPoints
                                               Input.OnChange (fun ev -> UpdateFormRewardPoints ev.Value |> dispatch)
                                               Input.Option.Props
                                                 [ OnKeyUp (fun ev -> if ev.keyCode = 13. then dispatch CreateTodoItem) ]
                                             ]
                                  Input.text [ Input.Placeholder "Deadline"
                                               Input.Value model.TodoForm.Deadline
                                               Input.OnChange (fun ev -> UpdateFormDeadline ev.Value |> dispatch)
                                               Input.Option.Props
                                                 [ OnKeyUp (fun ev -> if ev.keyCode = 13. then dispatch CreateTodoItem) ]
                                             ]
                                ]
                              Card.footer []
                                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> dispatch CreateTodoItem) ] ]
                                    [ str "Submit" ] ] ] ] ]
                  yield! model.TodoItems |> toCardRows dispatch ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// App
Program.mkSimple init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run
