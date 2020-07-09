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
  | Crops
  | Fertilizers
  | Buy
  | Sell
  | Replant
  | Settings
  | Debug
  static member List =
    [ Skills
      Crops
      Fertilizers
      Buy
      Sell
      Replant
      Settings
      Debug ]

type Model =
  { Page: Page
    Date: Date
    SidebarTab: SidebarTab
    SidebarOpen: bool
    Skills: Map<Name<Skill>, Skill>
    SkillList: Name<Skill> list
    IgnoreProfessions: bool
    BuySources: Map<Name<Source>, Source>
    BuySourceList: Name<Source> list
    SellSources: Map<Name<Processor>, Processor>
    SellSourceList: Name<Processor> list
    Fertilizers: Fertilizer list
    YearConditionsDo: ConditionsDo
    SkillLevelConditionsDo: ConditionsDo }
  member this.ConditionsMet conditions =
    List.forall
      (fun cond ->
        match cond with
        | SkillLevel sl -> this.Skills.[sl.Skill].Level >= sl.Level
        | Year y -> y <= this.Date.Year)
      conditions
  member this.ValidPrice price =
    match price.Override with
    | Some true -> true //ignore source conditions but consider local conditions 
    | Some false -> false //false, return display: manually overriden to false
    | None -> this.ConditionsMet price.Conditions && this.BuySources.[price.Source].Selected //conditions and source
  member this.BestPrices sources (priceFrom: Map<Name<Source>, Price>) =
    let validSources = List.filter (fun source -> this.ValidPrice priceFrom.[source]) sources
    if (validSources.IsEmpty) then
      []
    else 
      let min = priceFrom.[ (List.minBy (fun source -> priceFrom.[source].Value) validSources) ].Value
      List.filter (fun source -> priceFrom.[source].Value = min) validSources

let init () =
  let farmingProfessions =
    [ { Profession.initial with
          Name = "Tiller"
          UnlockLevel = 5
          Dependants = set [ Name "Artisan"; Name "Agriculturist" ] }
      { Profession.initial with
          Name = "Artisan"
          Requires = set [ Name "Tiller" ]
          ExclusiveWith = set [ Name "Agriculturist" ] }
      { Profession.initial with
          Name = "Agriculturist"
          Requires = set [ Name "Tiller" ]
          ExclusiveWith = set [ Name "Artisan" ] } ]
  let foragingProfessions =
    [ { Profession.initial with
          Name = "Gatherer"
          UnlockLevel = 5
          Dependants = set [ Name "Botanist" ] }
      { Profession.initial with
          Name = "Botanist"
          Requires = set [ Name "Gatherer" ] } ]
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
    [ { Source.initial with Name = "Pierre" }
      { Source.initial with Name = "Joja" }
      { Source.initial with Name = "Oasis" }
      { Source.initial with Name = "Traveling Merchant" }
      { Source.initial with Name = "Crafting" } ]
  let sellSourceList =
    [ { Processor.initial with
          Name = "Raw Crop"
          PreservesQuality = true }
      { Processor.initial with
          Name = "Preserves Jar"
          Conditions = [ SkillLevel {| Skill = Name "Farming"; Level = 4 |} ] }
      { Processor.initial with
          Name = "Keg"
          Conditions = [ SkillLevel {| Skill = Name "Farming"; Level = 8 |} ] }
      { Processor.initial with
          Name = "Oil Maker"
          Conditions = [ SkillLevel {| Skill = Name "Farming"; Level = 8 |} ] }
      { Processor.initial with Name = "Mill" }
      { Processor.initial with
          Name = "Seed Maker"
          Conditions = [ SkillLevel {| Skill = Name "Farming"; Level = 9 |} ] } ]
  let fertilizers =
    [ { Fertilizer.WithPrices
          [ { Price.initial with
                Value = 100
                Source = Name "Pierre" } ]
        with
          Name = "Basic Fertilizer"
          Quality = 1 }
      { Fertilizer.WithPrices
          [ { Price.initial with
                Value = 150
                Source = Name "Pierre"
                Conditions = [ Year 2 ] } ]
        with
          Name = "Quality Fertilizer"
          Quality = 2 }
      { Fertilizer.WithPrices
          [ { Price.initial with
                Value = 100
                Source = Name "Pierre" } ]
        with  
          Name = "Speed-Gro"
          Speed = 0.1 }
      { Fertilizer.WithPrices
          [ { Price.initial with
                Value = 150
                Source = Name "Pierre"
                Conditions = [ Year 2 ] }
            { Price.initial with
                Value = 80
                Source = Name "Oasis" } ]
        with
          Name = "Deluxe Speed-Gro"
          Speed = 0.25 }
       ]
  { Page = Home
    Date = Date.initial
    SidebarTab = Skills
    SidebarOpen = false
    Skills = 
      skillList
      |> List.map (fun s -> (Name s.Name, s))
      |> Map.ofList
    SkillList = List.map (fun (s: Skill) -> Name s.Name) skillList
    IgnoreProfessions = false
    BuySourceList = List.map (fun (s: Source) -> Name s.Name) buySourceList
    BuySources =
      buySourceList
      |> List.map (fun s -> (Name s.Name, s))
      |> Map.ofList
    SellSourceList = List.map (fun (s: Processor) -> Name s.Name) sellSourceList
    SellSources =
      sellSourceList
      |> List.map (fun s -> (Name s.Name, s))
      |> Map.ofList
    Fertilizers = fertilizers
    YearConditionsDo = Warn
    SkillLevelConditionsDo = Warn }

let initialModel = init ()

//--Update--
open Browser

type Message =
  | SetPage of Page
  | SetSidebarTab of SidebarTab
  | CloseSidebar
  | ToggleIgnoreProfessions
  | SetSkillLevel of Name<Skill> * int
  | SetSkillBuff of Name<Skill> * int
  | ToggleProfession of Skill * Name<Profession>
  | ToggleBuySource of Name<Source>
  | ToggleSellSource of Name<Processor>
  | ToggleFertSelected of Fertilizer
  | SetYear of int
  | SetYearConditionsDo of string
  | SetSkillLevelConditionsDo of string

//ran into trouble unboxing input from <select> element, so here we just parse the raw string:
let parseConditionsDo str =
  match str with
  | "Warn" -> Warn
  | "Override" -> Override
  | _ -> Ignore

let update message model =
  match message with
  | SetPage page -> { model with Page = page }
  | SetSidebarTab tab ->
      if (tab = model.SidebarTab) then
          { model with SidebarOpen = not model.SidebarOpen }
      else
          { model with SidebarTab = tab; SidebarOpen = true }
  | CloseSidebar -> { model with SidebarOpen = false }
  | ToggleIgnoreProfessions -> { model with IgnoreProfessions = not model.IgnoreProfessions }
  | SetSkillLevel (skill, level) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Level = min (max level 0) 10 } ) }
  | SetSkillBuff (skill, buff) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Buff = max buff 0 } ) }
  | ToggleProfession (skill, name) ->
      { model with Skills = model.Skills.Add(Name skill.Name, model.Skills.[Name skill.Name].ToggleProfession name model.IgnoreProfessions) }
  | ToggleBuySource source -> { model with BuySources = model.BuySources.Add(source, model.BuySources.[source].Toggle) }
  | ToggleSellSource source -> { model with SellSources = model.SellSources.Add(source, model.SellSources.[source].Toggle) }
  | ToggleFertSelected fert -> { model with Fertilizers = List.map (fun f -> if f = fert then f.Toggle else f ) model.Fertilizers}
  | SetYear year -> { model with Date = { model.Date with Year = year } }
  | SetYearConditionsDo something -> { model with YearConditionsDo = parseConditionsDo something }
  | SetSkillLevelConditionsDo something -> { model with SkillLevelConditionsDo = parseConditionsDo something }

//--View--
open Fable.React
open Fable.React.Props
open Elmish.React.Common
open Elmish.React.Helpers

let classModifier baseClass modifier apply =
  ClassName (if apply then baseClass + "--" + modifier else baseClass)

let checkboxOld text message isChecked dispatch =
  label [ ClassName "checkbox-label" ]
    [ input
        [ Type "checkbox"
          Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
          Checked isChecked
          OnChange (fun _ -> dispatch message) ]
      span [ ClassName "checkbox" ]
        [ span [ classModifier "checkmark" "active" isChecked ] [] ]
      str text ]

let checkbox message isChecked dispatch =
  label [ ClassName "checkbox-img-label" ]
    [ input
        [ Type "checkbox"
          Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
          Checked isChecked
          OnChange (fun _ -> dispatch message) ]
      img
        [ ClassName "checkbox-img"
          Src (if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ] ]

let settingCheckbox text message isChecked dispatch =
  label [ ClassName "checkbox-img-label" ]
    [ input
        [ Type "checkbox"
          Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
          Checked isChecked
          OnChange (fun _ -> dispatch message) ]
      img
        [ ClassName "checkbox-img"
          Src (if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ]
      str text ]

let checkboxImg isChecked status =
  if isChecked then
    match status with
    | Valid -> "img/UI/CheckboxGreen.png"
    | Warning -> "img/UI/CheckboxYellow.png"
    | Invalid -> "img/UI/CheckboxRed.png"
  else
    "img/UI/Checkbox.png"

let statusCheckbox siblings message isChecked status dispatch =
  label [ ClassName "checkbox-img-label" ]
    ( ( [ input
            [ Type "checkbox"
              Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
              Checked isChecked
              OnChange (fun _ -> dispatch message) ]
          img
            [ ClassName "checkbox-img"
              Src (checkboxImg isChecked status) ] ] )
      @ siblings)

let sameTabAs tab currentTab = tab = currentTab

let viewTab css message tab active dispatch =
  li
    [ classModifier (css + "-tab") "active" active
      OnClick (fun _ -> dispatch <| message tab) ]
    [ str (tab.ToString()) ]

let viewTabs css message list activeFun dispatch =
  ul [ ClassName (css + "-tabs") ]
    [ for tab in list do
        viewTab css message tab (activeFun tab) dispatch ]

//let lazySidebarContent =

let skillLevelInput name level dispatch =
  let levelInput mode name level dispatch =
    input
      [ Type mode
        Min 0
        Max 10
        valueOrDefault level
        ClassName ("skill-" + mode + "-input")
        OnChange (fun l -> dispatch <| SetSkillLevel (name, !!l.Value)) ]
  label [ ClassName "skill-input" ]
    [ str "Level: "
      levelInput "range" name level dispatch
      levelInput "number" name level dispatch ]

let skillBuffInput (name: Name<Skill>) buff dispatch =
  label [ ClassName "skill-input" ]
    [ str "Buff: "
      input
        [ Type "number"
          Min 0
          valueOrDefault buff
          ClassName "skill-number-input"
          OnChange (fun b -> dispatch <| SetSkillBuff (name, !!b.Value)) ] ]

let profession conditionsDo name skill dispatch =
  console.log(conditionsDo)
  button
    [ classModifier "profession" "active" skill.Professions.[name].Selected
      OnClick (fun _ -> dispatch <| ToggleProfession (skill, name)) ]
    [ img
        [ ClassName "profession-img"
          Src ("img/Skills/" + name.Value + ".png")]
      str name.Value
      ofOption(
        if not skill.Professions.[name].Selected || skill.ProfessionUnlocked name then
          None
        else
          match conditionsDo with
          | Warn -> Some (span [] [ str " warn" ])
          | Override -> Some (span [] [ str " override"])
          | Ignore -> None) ]
let professionRow conditionsDo skill row dispatch =
  div [ ClassName "profession-row" ]
    [ for name in row do
        profession conditionsDo name skill dispatch ]
let viewProfessions conditionsDo skill dispatch =
  div [ ClassName "professions" ]
    [ for row in skill.ProfessionLayout do
        professionRow conditionsDo skill row dispatch ]

let viewSources message lens list (map: Map<Name<'t>, 't>) dispatch =
  let source message (name: Name<'t>) active dispatch =
    li []
      [ statusCheckbox
          [ img
              [ ClassName "source-img" 
                Src ("img/Sources/" + name.Value + ".png") ]
            str name.Value ]
          (message name)
          active
          Valid
          dispatch ]
  ul [ ClassName "source-list" ]
    [ for name in list do
        source message name (lens map.[name]) dispatch ]

let selectConditionsDo text message value dispatch =
  label []
      [ str (text + ": ")
        select
          [ valueOrDefault value
            OnChange (fun x -> dispatch <| message !!x.Value) ]
          [ for something in ConditionsDo.List do
              option
                [ Value something ]
                [ str (string something) ] ] ]

let sidebarContent model dispatch =
  div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
    ( match model.SidebarTab with
    | Skills ->
      [ ul [ ClassName "skills" ]
          [ for skill in model.SkillList do
              li [ ClassName "skill" ]
                [ span [ ClassName "skill-header" ]
                    [ img
                        [ ClassName "skill-img"
                          Src ("/img/Skills/" + skill.Value + ".png")]
                      str model.Skills.[skill].Name ]
                  skillLevelInput skill model.Skills.[skill].Level dispatch
                  skillBuffInput skill model.Skills.[skill].Buff dispatch
                  viewProfessions model.SkillLevelConditionsDo model.Skills.[skill] dispatch ] ]
        settingCheckbox "Ignore Profession Relationships" ToggleIgnoreProfessions model.IgnoreProfessions dispatch ]
    | Crops -> [ span [] [] ]
    | Fertilizers ->
      [ table [ ClassName "fertilizers" ]
          [ thead []
              [ tr []
                  [ th [] []
                    th [] [ str "Fertilizer" ]
                    th [] [ str "Quality" ]
                    th [] [ str "Speed" ]
                    th [] [ str "Price" ] ] ]
            tbody []
              [ for fert in model.Fertilizers do
                  tr []
                    [ td [] [ checkbox (ToggleFertSelected fert) fert.Selected dispatch ] 
                      td []
                        [ img
                            [ ClassName "fertilizer-img"
                              Src ("/img/Fertilizers/" + fert.Name + ".png") ]
                          str fert.Name ]
                      td [] [ ofInt fert.Quality ]
                      td [] [ ofFloat fert.Speed ]
                      td [] [ match model.BestPrices fert.Sources fert.PriceFrom with 
                              | [] -> str "None"
                              | list -> ofInt fert.PriceFrom.[list.Head].Value ] ] ] ] ] //price
    | Buy ->
      [ viewSources ToggleBuySource (fun s -> s.Selected) model.BuySourceList model.BuySources dispatch ]
    | Sell ->
      [ viewSources ToggleSellSource (fun s -> s.Selected) model.SellSourceList model.SellSources dispatch ]
    | Replant
    | Settings ->
      [ selectConditionsDo "Year Conditions" SetYearConditionsDo model.YearConditionsDo dispatch
        selectConditionsDo "Skill Level Conditions" SetSkillLevelConditionsDo model.SkillLevelConditionsDo dispatch ]
    | Debug ->
      [ input
          [ Type "number"
            OnClick (fun y -> dispatch <| (SetYear !!y.Value) )
            valueOrDefault model.Date.Year ] ] )
  //lazyView2With (fun oldModel newModel -> (not oldModel.SidebarOpen && not newModel.SidebarOpen) || oldModel = newModel) sidebarContent

let cover sidebarOpen dispatch =
  div
    [ classModifier "cover" "open" sidebarOpen
      OnClick (fun _ -> dispatch CloseSidebar) ]
    []

let sidebar model dispatch =
  div [ classModifier "sidebar" "open" model.SidebarOpen ]
    [ //lazySidebarContent model dispatch
      sidebarContent model dispatch
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
        cover model.SidebarOpen dispatch
        sidebar model dispatch ]

//--App--
open Elmish.React
open Elmish.Debug
open Elmish.HMR

Program.mkSimple (fun _ -> initialModel) update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run