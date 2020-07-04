module App

open Fable.Core
open Fable.Core.JsInterop
open Elmish

importAll "../sass/main.sass"

//--Model--
open Types

type Page =
    | Home
    | Help

type SidebarTab =
    | Skills
    | Buy
    | Sell
    | Replant
    | Settings
    static member List =
        [ Skills
          Buy
          Sell
          Replant
          Settings ]

type SourceTab =
    | Global
    | Individual

type Model =
    { Page: Page
      SidebarTab: SidebarTab
      SidebarOpen: bool
      Skills: Map<Name<Skill>, Skill>
      SkillTab: Name<Skill>
      IgnoreConflicts: bool
      BuySources: Map<Name<Source>, Source>
      BuySourceList: Name<Source> list
      BuyTab: SourceTab
      SellSources: Map<Name<Processor>, Processor>
      SellSourceList: Name<Processor> list
      SellTab: SourceTab
      ReplantTab: SourceTab
      Test: bool }
    member this.Skill = this.Skills.[this.SkillTab]

let init () =
    let farmingProfessions =
        [ { Name = "Tiller"
            Selectable = Selectable.initial
            Requires = Set.empty
            ExclusiveWith = Set.empty
            Dependants = set [ Name "Artisan"; Name "Agriculturist" ] }
          { Name = "Artisan"
            Selectable = Selectable.initial
            Requires = set [ Name "Tiller" ]
            ExclusiveWith = set [ Name "Agriculturist" ]
            Dependants = Set.empty }
          { Name = "Agriculturist"
            Selectable = Selectable.initial
            Requires = set [ Name "Tiller" ]
            ExclusiveWith = set [ Name "Artisan" ]
            Dependants = Set.empty } ]
    let foragingProfessions =
        [ { Name = "Gatherer"
            Selectable = Selectable.initial
            Requires = Set.empty
            ExclusiveWith = Set.empty
            Dependants = set [ Name "Botanist" ] }
          { Name = "Botanist"
            Selectable = Selectable.initial
            Requires = set [ Name "Gatherer"  ]
            ExclusiveWith = Set.empty
            Dependants = Set.empty } ]
    let listToNameMap (list: Profession list) =
        list
        |> List.map (fun p -> (Name p.Name, p))
        |> Map.ofList
    let skillList = 
        [ { Skill.initial with
                Name = "Farming"
                Professions = listToNameMap farmingProfessions
                ProfessionLayout =
                    [ [ Name "Tiller" ]
                      [ Name "Artisan"; Name "Agriculturist" ] ] }
          { Skill.initial with
                Name = "Foraging"
                Professions = listToNameMap foragingProfessions
                ProfessionLayout = [ [ Name "Gatherer" ]; [ Name "Botanist" ] ] } ]
    let buySourceList =
        [ { Name = "Pierre"
            Selectable = Selectable.initial }
          { Name = "Joja"
            Selectable = Selectable.initial }
          { Name = "Oasis"
            Selectable = Selectable.initial }
          { Name = "Traveling Merchant"
            Selectable = Selectable.initial }
          { Name = "Crafting"
            Selectable = Selectable.initial } ]
    let sellSourceList =
        [ { Name = "Raw Crop"
            Selectable = Selectable.initial
            PreservesQuality = true }
          { Name = "Preserves Jar"
            Selectable = Selectable.initial
            PreservesQuality = false } 
          { Name = "Keg"
            Selectable = Selectable.initial
            PreservesQuality = false }
          { Name = "Oil Maker"
            Selectable = Selectable.initial
            PreservesQuality = false }
          { Name = "Mill"
            Selectable = Selectable.initial
            PreservesQuality = false }
          { Name = "Seed Maker"
            Selectable = Selectable.initial
            PreservesQuality = false } ]
    { Page = Home
      SidebarTab = Skills
      SidebarOpen = false
      Skills = 
        skillList
        |> List.map (fun s -> (Name s.Name, s))
        |> Map.ofList
      SkillTab = Name skillList.Head.Name
      IgnoreConflicts = false
      BuySourceList = List.map (fun (s: Source) -> Name s.Name) buySourceList
      BuySources =
        buySourceList
        |> List.map (fun s -> (Name s.Name, s))
        |> Map.ofList
      BuyTab = Global
      SellSourceList = List.map (fun (s: Processor) -> Name s.Name) sellSourceList
      SellSources =
        sellSourceList
        |> List.map (fun s -> (Name s.Name, s))
        |> Map.ofList
      SellTab = Global
      ReplantTab = Global
      Test = false }

let initialModel = init ()

//--Update--
open Browser

type Message =
    | SetPage of Page
    | SetSidebarTab of SidebarTab
    | CloseSidebar
    | SetSkillTab of Name<Skill>
    | ToggleIgnoreConflicts
    | SetSkillLevel of int
    | SetSkillBuff of int
    | ToggleProfession of Name<Profession>
    | SetSourceTab of SourceTab
    | ToggleBuySource of Name<Source>
    | ToggleSellSource of Name<Processor>
    | TestToggle

let update message model =
    match message with
    | SetPage page -> { model with Page = page }
    | SetSidebarTab tab ->
        if (tab = model.SidebarTab) then
            { model with SidebarOpen = not model.SidebarOpen }
        else
            { model with SidebarTab = tab; SidebarOpen = true }
    | CloseSidebar -> { model with SidebarOpen = false }
    | SetSkillTab tab -> { model with SkillTab = tab }
    | ToggleIgnoreConflicts -> { model with IgnoreConflicts = not model.IgnoreConflicts }
    | SetSkillLevel level ->
        { model with Skills = model.Skills.Add(model.SkillTab, { model.Skill with Level = min (max level 0) 10 } ) }
    | SetSkillBuff buff ->
        { model with Skills = model.Skills.Add(model.SkillTab, { model.Skill with Buff = max buff 0 } ) }
    | ToggleProfession name ->
        { model with Skills = model.Skills.Add(model.SkillTab, model.Skill.ToggleProfession name model.IgnoreConflicts) }
    | SetSourceTab tab ->
        match model.SidebarTab with
        | Buy -> { model with BuyTab = tab }
        | Sell -> { model with SellTab = tab }
        | Replant -> { model with ReplantTab = tab }
        | _ ->
            console.log("Something that should be impossible happened: the source tab was changed while on a sidebar tab that does not have source tabs.")
            model
    | ToggleBuySource source -> { model with BuySources = model.BuySources.Add(source, model.BuySources.[source].Toggle) }
    | ToggleSellSource source -> { model with SellSources = model.SellSources.Add(source, model.SellSources.[source].Toggle) }
    | TestToggle -> { model with Test = not model.Test }

//--View--
open Fable.React
open Fable.React.Props
open Elmish.React.Common
open Elmish.React.Helpers

let classes list =
    list
    |> Seq.fold (fun state name -> state + " " + name) ""
    |> ClassName

let classFlag baseClass modifier apply =
    ClassName (if apply then baseClass + "--" + modifier else baseClass)
//Here begins the over-optimised lazyViews...

let checkbox message text isChecked dispatch =
    label [ ClassName "checkbox-label" ]
        [ input
            [ Type "checkbox"
              Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
              Checked isChecked
              OnChange (fun _ -> dispatch message) ]
          span [ ClassName "checkbox" ]
            [ span [ classFlag "checkmark" "active" isChecked ] [] ]
          text ]

//A custom, slow? font is being used, so let's lazyView the text as well cause we can.
let lazyStr = lazyView str

let lazyCover =
    let cover sidebarOpen dispatch =
        div
            [ classFlag "cover" "open" sidebarOpen
              OnClick (fun _ -> dispatch CloseSidebar) ]
            []
    lazyView2 cover
  
let lazySidebar =
    let lazySidebarContent =

        let lazySkillTabs =
            let lazySkillTab =
                let skillTab name active dispatch =
                    li 
                        [ classFlag "skill-tab" "active" active
                          OnClick (fun _ -> if active then () else dispatch <| SetSkillTab name)]
                        [ lazyStr name.Value ]
                lazyView3 skillTab
            let skillTabs currentSkillTab dispatch =
                ul [ ClassName "skill-tabs" ]
                    [ for KeyValue(tab, _) in initialModel.Skills do
                        lazySkillTab tab (tab = currentSkillTab) dispatch]
            lazyView2 skillTabs

        let lazySkillLevelInput =
            console.log("fun")
            let levelInput mode level dispatch =
                input
                    [ Type mode
                      Min 0
                      Max 10
                      valueOrDefault level
                      ClassName "number-input"
                      OnChange (fun l -> dispatch <| SetSkillLevel !!l.Value) ]
            let lazyRangeInput = lazyView2 (levelInput "range")
            let lazyNumberInput = lazyView2 (levelInput "number")
            let skillLevelInput (skill: Skill) dispatch =
                label [ ClassName "input-label" ]
                    [ lazyStr (skill.Name + " Level: ")
                      lazyRangeInput skill.Level dispatch
                      lazyNumberInput skill.Level dispatch ]
            lazyView2 skillLevelInput

        let lazySkillBuffInput =
            let lazyBuffInput =
                let buffInput buff dispatch =
                    input
                        [ Type "number"
                          Min 0
                          valueOrDefault buff
                          ClassName "number-input"
                          OnChange (fun b -> dispatch <| SetSkillBuff !!b.Value) ]
                lazyView2 buffInput
            let skillBuffInput (skill: Skill) dispatch =
                label [ ClassName "input-label" ]
                    [ lazyStr (skill.Name + " Buff: ")
                      lazyBuffInput skill.Buff dispatch ]
            lazyView2 skillBuffInput

        let lazyProfessions =
            let lazyProfession = 
                let profession name selected dispatch =
                    button
                        [ classFlag "profession" "active" selected
                          OnClick (fun _ -> dispatch <| ToggleProfession name) ]
                        [ lazyStr name.Value ]
                lazyView3 profession
            let professions (layout: Name<Profession> list list) professions dispatch =
                let professionRow list (professions: Map<Name<Profession>, Profession>) dispatch =
                    div []
                        [ for name in list do
                            lazyProfession name professions.[name].Selectable.Selected dispatch ]
                div []
                    [ for list in layout do
                        professionRow list professions dispatch ]
            lazyView3 professions

        let lazyIgnoreConflicts = lazyView2 (checkbox ToggleIgnoreConflicts (lazyStr "Ignore Profession Conflicts"))

        let lazySourceTabs =
            let lazySourceTab =
                let sourceTab tab active dispatch =
                    li
                        [ classFlag "source-tab" "active" active
                          OnClick (fun _ -> if active then () else dispatch <| SetSourceTab tab) ]
                        [ lazyStr (string tab) ]
                lazyView3 sourceTab
            let sourceTabs currentTab dispatch =
                ul [ ClassName "source-tabs" ]
                    [ lazySourceTab Global (Global = currentTab) dispatch
                      lazySourceTab Individual (Individual = currentTab) dispatch ]
            lazyView2 sourceTabs

        let lazySources message lens list =
            let lazySource message =
                let source message (name: Name<'t>) active dispatch =
                    li []
                        [ checkbox (message name) (lazyStr name.Value) active dispatch ]
                lazyView3 (source message)
            let sources message lens list sourceTab (map: Map<Name<'t>, 't>) dispatch =
                match sourceTab with
                | Global ->
                    ul [ ClassName "source-list" ]
                        [ for name in list do
                            lazySource message name (lens map.[name]) dispatch ]
                | Individual ->
                    span [] [ str "Todo" ]
            lazyView3 (sources message lens list)

        let lazyBuySources = lazySources ToggleBuySource Source.Selected initialModel.BuySourceList
        let lazySellSources = lazySources ToggleSellSource Processor.Selected initialModel.SellSourceList

        let sidebarContent model dispatch =
            div [ classFlag "sidebar-content" "open" model.SidebarOpen ]
                ( match model.SidebarTab with
                | Skills ->
                  [ lazySkillTabs model.SkillTab dispatch
                    br []
                    lazySkillLevelInput model.Skill dispatch
                    br []
                    lazySkillBuffInput model.Skill dispatch
                    br []
                    lazyProfessions model.Skill.ProfessionLayout model.Skill.Professions dispatch
                    br []
                    lazyIgnoreConflicts model.IgnoreConflicts dispatch ]
                | Buy ->
                  [ lazySourceTabs model.BuyTab dispatch
                    br []
                    lazyBuySources model.BuyTab model.BuySources dispatch ]
                | Sell ->
                   [ lazySourceTabs model.SellTab dispatch 
                     br []
                     lazySellSources model.SellTab model.SellSources dispatch ]
                | Replant
                | Settings -> [ span [] [] ] )
        lazyView2With (fun oldModel newModel -> (not oldModel.SidebarOpen && not newModel.SidebarOpen) || oldModel = newModel) sidebarContent

    let lazySidebarTabs =
        let lazySidebarTab =
            let sidebarTab tab active dispatch =
                li
                    [ classFlag "sidebar-tab" "active" active
                      OnClick (fun _ -> dispatch <| SetSidebarTab tab) ]
                    [ lazyStr (string tab) ]
            lazyView3 sidebarTab
        let sidebarTabs sidebarOpen currentTab dispatch =
            ul [ ClassName "sidebar-tabs" ]
                [ for tab in SidebarTab.List do
                    lazySidebarTab tab (sidebarOpen && tab = currentTab) dispatch ]
        lazyView3 sidebarTabs

    let sidebar model dispatch =
        div [ classFlag "sidebar" "open" model.SidebarOpen ]
            [ lazySidebarContent model dispatch
              lazySidebarTabs model.SidebarOpen model.SidebarTab dispatch ]
    lazyView2 sidebar

let view model dispatch =
    match model.Page with
    | Help ->
        span [] [ str "Help! I need somebody!" ]
    | Home ->
        div []
            [ div [ Class "calender-grid" ]
                []
              button []
                [ str "Help" ]
              input [ Type "checkbox"; Checked model.Test; OnChange (fun _ -> dispatch TestToggle) ]
              lazyCover model.SidebarOpen dispatch
              lazySidebar model dispatch ]

//--App--
open Elmish.React
open Elmish.Debug

Program.mkSimple (fun _ -> initialModel) update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run

console.log("test")