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
    static member List = [ Global; Individual ]

type Model =
    { Page: Page
      SidebarTab: SidebarTab
      SidebarOpen: bool
      Skills: Map<Name<Skill>, Skill>
      SkillList: Name<Skill> list
      IgnoreConflicts: bool
      BuySources: Map<Name<Source>, Source>
      BuySourceList: Name<Source> list
      BuyTab: SourceTab
      SellSources: Map<Name<Processor>, Processor>
      SellSourceList: Name<Processor> list
      SellTab: SourceTab
      ReplantTab: SourceTab
      Test: bool }

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
      SkillList = List.map (fun (s: Skill) -> Name s.Name) skillList
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
    | SetSkillLevel of Name<Skill> * int
    | SetSkillBuff of Name<Skill> * int
    | ToggleProfession of Skill * Name<Profession>
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
    | ToggleIgnoreConflicts -> { model with IgnoreConflicts = not model.IgnoreConflicts }
    | SetSkillLevel (skill, level) ->
        { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Level = min (max level 0) 10 } ) }
    | SetSkillBuff (skill, buff) ->
        { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Buff = max buff 0 } ) }
    | ToggleProfession (skill, name) ->
        { model with Skills = model.Skills.Add(Name skill.Name, model.Skills.[Name skill.Name].ToggleProfession name model.IgnoreConflicts) }
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

let classModifier baseClass modifier apply =
    ClassName (if apply then baseClass + "--" + modifier else baseClass)

let checkbox text message isChecked dispatch =
    label [ ClassName "checkbox-label" ]
        [ input
            [ Type "checkbox"
              Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
              Checked isChecked
              OnChange (fun _ -> dispatch message) ]
          span [ ClassName "checkbox" ]
            [ span [ classModifier "checkmark" "active" isChecked ] [] ]
          str text ]

let sameTabAs tab currentTab = tab = currentTab

let viewTabs css message list activeFun dispatch =
    let viewTab css message tab active dispatch =
        li
            [ classModifier (css + "-tab") "active" active
              OnClick (fun _ -> dispatch <| message tab) ]
            [ str (tab.ToString()) ]
    ul [ ClassName (css + "-tabs") ]
        [ for tab in list do
            viewTab css message tab (activeFun tab) dispatch ]

let lazySidebarContent =

    let skillLevelInput name level dispatch =
        let levelInput mode name level dispatch =
            input
                [ Type mode
                  Min 0
                  Max 10
                  valueOrDefault level
                  ClassName ("skill-" + mode + "-input")
                  OnChange (fun l -> dispatch <| SetSkillLevel (name, !!l.Value)) ]
        label [ ClassName "skill-input-label" ]
            [ str "Level: "
              levelInput "range" name level dispatch
              levelInput "number" name level dispatch ]

    let skillBuffInput name buff dispatch =
        label [ ClassName "skill-input-label" ]
            [ str "Buff: "
              input
                [ Type "number"
                  Min 0
                  valueOrDefault buff
                  ClassName "skill-number-input"
                  OnChange (fun b -> dispatch <| SetSkillBuff (name, !!b.Value)) ] ]

    let viewProfessions skill dispatch =
        let professionRow skill row dispatch =
            let profession name selected dispatch =
                button
                    [ classModifier "profession" "active" selected
                      OnClick (fun _ -> dispatch <| ToggleProfession (skill, name)) ]
                    [ str name.Value ]
            div [ ClassName "profession-row" ]
                [ for name in row do
                    profession name skill.Professions.[name].Selectable.Selected dispatch ]
        div [ ClassName "professions" ]
            [ for row in skill.ProfessionLayout do
                professionRow skill row dispatch ]

    let viewSources message lens list sourceTab (map: Map<Name<'t>, 't>) dispatch =
        let source message (name: Name<'t>) active dispatch =
            li []
                [ checkbox name.Value (message name) active dispatch ]
        match sourceTab with
        | Global ->
            ul [ ClassName "source-list" ]
                [ for name in list do
                    source message name (lens map.[name]) dispatch ]
        | Individual ->
            span [] [ str "Todo" ]

    let sidebarContent model dispatch =
        div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
            ( match model.SidebarTab with
            | Skills ->
              [ for skill in model.SkillList do
                    div [ ClassName "skill" ]
                        [ str model.Skills.[skill].Name
                          div [ ClassName "skill-inputs" ]
                              [ skillLevelInput skill model.Skills.[skill].Level dispatch
                                skillBuffInput skill model.Skills.[skill].Buff dispatch ]
                          viewProfessions model.Skills.[skill] dispatch ]
                checkbox "Ignore Profession Conflicts" ToggleIgnoreConflicts model.IgnoreConflicts dispatch ]
            | Buy ->
              [ viewTabs "source" SetSourceTab SourceTab.List (sameTabAs model.BuyTab) dispatch
                viewSources ToggleBuySource Source.Selected model.BuySourceList model.BuyTab model.BuySources dispatch ]
            | Sell ->
               [ viewTabs "source" SetSourceTab SourceTab.List (sameTabAs model.SellTab) dispatch 
                 viewSources ToggleSellSource Processor.Selected model.SellSourceList model.SellTab model.SellSources dispatch ]
            | Replant
            | Settings -> [ span [] [] ] )
    lazyView2With (fun oldModel newModel -> (not oldModel.SidebarOpen && not newModel.SidebarOpen) || oldModel = newModel) sidebarContent

let cover sidebarOpen dispatch =
    div
        [ classModifier "cover" "open" sidebarOpen
          OnClick (fun _ -> dispatch CloseSidebar) ]
        []

let sidebar model dispatch =
    div [ classModifier "sidebar" "open" model.SidebarOpen ]
        [ lazySidebarContent model dispatch
          viewTabs "sidebar" SetSidebarTab SidebarTab.List (fun t -> model.SidebarOpen && t = model.SidebarTab) dispatch ]

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
              cover model.SidebarOpen dispatch
              sidebar model dispatch ]

//--App--
open Elmish.React
open Elmish.Debug

Program.mkSimple (fun _ -> initialModel) update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run