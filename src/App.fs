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
    BuySourceList: Name<Source> list //necessary to preserve order, haven't thought of another way
    SellSources: Map<Name<Processor>, Processor>
    SellSourceList: Name<Processor> list //same thing here ^^
    Fertilizers: Fertilizer list
    YearConditionsDo: ConditionsDo
    SkillLevelConditionsDo: ConditionsDo }
  member this.ConditionMet = function
    | SkillLevel sl -> this.SkillLevelConditionsDo = Ignore || this.Skills.[sl.Skill].Level >= sl.Level
    | Year y -> this.YearConditionsDo = Ignore || this.Date.Year >= y
  member this.ConditionsMet = List.forall this.ConditionMet
  member this.UnmetConditions = List.filter (this.ConditionMet >> not)
  member this.ConditionStatusData conditions overallStatus =
    conditions
    |> List.filter (fun c -> this.ConditionStatus c = overallStatus)
    |> List.map InvalidCondition
  member private this.StatusHelper condition conditionsDo =
    match conditionsDo with
    | Ignore -> Valid
    | Warn | Invalidate when (this.ConditionMet condition) -> Valid
    | Warn -> Warning
    | Invalidate -> Invalid
  member this.ConditionStatus = function
    | SkillLevel sl -> this.StatusHelper (SkillLevel sl) this.SkillLevelConditionsDo
    | Year y -> this.StatusHelper (Year y) this.YearConditionsDo
  member this.ConditionStatuses = List.map (fun c -> (c, this.ConditionStatus c))
  member this.OverallStatus conditionStatuses =
    if (List.exists (fun cs -> snd cs = Invalid) conditionStatuses) then
      Invalid
    elif (List.exists (fun cs -> snd cs = Warning) conditionStatuses) then
      Warning
    else
      Valid
  member this.PriceStatus price =
    match price.Override with
    | Some true -> Valid //true //ignore source conditions but consider local conditions 
    | Some false -> Invalid //false //false, return display: manually overriden to false
    | None ->
        if (this.BuySources.[price.Source].Selected) then
          this.ConditionStatuses price.Conditions
          |> this.OverallStatus
        else
          Invalid
  member this.BestPrices sources (priceFrom: Map<Name<Source>, Price>) =
    let priceStatuses = List.map (fun source -> priceFrom.[source], this.PriceStatus priceFrom.[source]) sources
    let validPrices = List.filter (fun ps -> snd ps <> Invalid) priceStatuses
    if (validPrices.IsEmpty) then
      priceStatuses
    else
      let bestPrice = 
        (validPrices
        |> List.minBy (fun ps -> (fst ps).Value)
        |> fst).Value
      List.filter (fun ps -> (fst ps).Value = bestPrice) validPrices
  member this.SourceStatus (source: Processor) =
    this.ConditionStatuses source.Conditions
    |> this.OverallStatus
  member this.SourceStatusData (source: Name<Processor>) overallStatus =
    this.ConditionStatusData this.SellSources.[source].Conditions overallStatus
  member this.PriceStatusData (priceStatus: Price * Status) =
    ( if (snd priceStatus = Invalid) then
        match (fst priceStatus).Override with
        | Some true -> []
        | Some false -> []
        | None ->
            if (not this.BuySources.[(fst priceStatus).Source].Selected) then
              [ Reason "Is not selected." ]
            else []
      else [] )
    @ this.ConditionStatusData (fst priceStatus).Conditions (snd priceStatus)

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
    BuySourceList = List.map (fun (s: Source)-> Name s.Name) buySourceList
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
  | SetYearConditionsDo of ConditionsDo
  | SetSkillLevelConditionsDo of ConditionsDo

//ran into trouble unboxing input from <select> element, so here we just parse the raw string:
let parseConditionsDo str =
  match str with
  | "Warn" -> Warn
  | "Invalidate" -> Invalidate
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
  | SetYearConditionsDo something -> { model with YearConditionsDo = something }
  | SetSkillLevelConditionsDo something -> { model with SkillLevelConditionsDo = something }

//--View--
open Fable.React
open Fable.React.Props
open Elmish.React.Common
open Elmish.React.Helpers

let classModifier baseClass modifier apply =
  ClassName (if apply then baseClass + "--" + modifier else baseClass)

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

let checkboxWith alsoDisplay message isChecked dispatch =
  label [ ClassName "checkbox-img-label" ]
    ( [ input
          [ Type "checkbox"
            Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
            Checked isChecked
            OnChange (fun _ -> dispatch message) ]
        img
          [ ClassName "checkbox-img"
            Src (if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ] ]
      @alsoDisplay )

let checkboxText text message isChecked dispatch =
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

let statusCheckbox displayAfter message isChecked status dispatch =
  label [ ClassName "checkbox-img-label" ]
    ( [ input
          [ Type "checkbox"
            Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
            Checked isChecked
            OnChange (fun _ -> dispatch message) ]
        img
          [ ClassName "checkbox-img"
            Src (checkboxImg isChecked status) ] ]
      @ displayAfter)

let viewTab css message tab active dispatch =
  li
    [ classModifier (css + "-tab") "active" active
      OnClick (fun _ -> dispatch <| message tab) ]
    [ str (string tab) ]

let viewTabs css message list activeFun dispatch =
  ul [ ClassName (css + "-tabs") ]
    [ for tab in list do
        viewTab css message tab (activeFun tab) dispatch ]

//let lazySidebarContent =

let levelInput mode name level dispatch =
  input
    [ Type mode
      Min 0
      Max 10
      valueOrDefault level
      ClassName ("skill-" + mode + "-input")
      OnChange (fun l -> dispatch <| SetSkillLevel (name, !!l.Value)) ]

let skillLevelInput name level dispatch =
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
  button
    [ classModifier "profession" "active" skill.Professions.[name].Selected
      OnClick (fun _ -> dispatch <| ToggleProfession (skill, name)) ]
    [ img
        [ ClassName "profession-img"
          Src ("img/Skills/" + name.Value + ".png")]
      str name.Value
      if skill.Professions.[name].Selected && not (skill.ProfessionUnlocked name) then
        if conditionsDo = Warn then
          span [] [ str " warn" ]
        elif conditionsDo = Invalidate then
          span [] [ str " override"] ]

let professionRow conditionsDo skill row dispatch =
  div [ ClassName "profession-row" ]
    [ for name in row do
        profession conditionsDo name skill dispatch ]

let viewProfessions conditionsDo skill dispatch =
  div [ ClassName "professions" ]
    [ for row in skill.ProfessionLayout do
        professionRow conditionsDo skill row dispatch ]

let sourceIcon (name: Name<'t>) =
  [ img
      [ ClassName "source-img" 
        Src ("img/Sources/" + name.Value + ".png") ]
    str name.Value ]

let rec invalidReason reason =
  li []
    [ match reason with
      | InvalidCondition c ->
          match c with
          | SkillLevel sl -> str (sl.Skill.Value + " level too low. Unlocks at level " + string sl.Level + ".")
          | Year y -> str ("Available only from year " + string y + " and onwards.")
      | Reason reason -> str reason
      | SubReason (text, sub) -> str text; for subReason in sub do ul [] [ invalidReason subReason ] ]

let buySource (name: Name<Source>) selected dispatch =
  li []
    [ checkboxWith (sourceIcon name) (ToggleBuySource name) selected dispatch ]

let buySources list (sources: Map<Name<Source>, Source>) dispatch =
  ul [ClassName "source-list" ]
    [ for name in list do
        buySource name sources.[name].Selected dispatch ]

let sellSource (name: Name<Processor>) selected status dispatch =
  li []
    [ statusCheckbox (sourceIcon name) (ToggleSellSource name) selected status dispatch ]

let sellSources model dispatch =
  ul [ ClassName "source-list" ]
    [ for name in model.SellSourceList do
        let status = model.SourceStatus model.SellSources.[name]
        sellSource name model.SellSources.[name].Selected status dispatch
        if status = Warning then
          let id = System.Guid.NewGuid().ToString()
          label
            [ ClassName "details-label"
              HtmlFor id ]
            [ str "Show Warnings"]
          input
            [ Type "checkbox"
              ClassName "details-input"
              Id id ]
          ul [ ClassName "details" ]
            [ for reason in model.SourceStatusData name status do
                invalidReason reason ] 
        elif status = Invalid then
          let id = System.Guid.NewGuid().ToString()
          label
            [ ClassName "details-label"
              HtmlFor id ]
            [ str "Show Errors"]
          input
            [ Type "checkbox"
              ClassName "details-input"
              Id id ]
          ul [ ClassName "details" ]
            [ for reason in model.SourceStatusData name status do
                invalidReason reason ] ]

let selectConditionsDo text message value dispatch=
  label []
      [ str (text + ": ")
        select
          [ valueOrDefault value
            OnChange (fun x -> parseConditionsDo x.Value |> message |> dispatch) ]
          [ for something in ConditionsDo.List do
              option
                [ Value something ]
                [ str (string something) ] ] ]

let viewPrices (model: Model) (priceStatuses: (Price * Status) list) =
  let status =
    if (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
      Warning
    elif (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
      Valid
    else
      Invalid
  match status with
  | Warning ->
      let id = System.Guid.NewGuid().ToString()
      [ ofInt (fst priceStatuses.Head).Value
        for price in priceStatuses do
          img
            [ classModifier "price-img" "warning" (snd price = Warning)
              Src ("img/Sources/" + (fst price).Source.Value + ".png") ]
        br []
        img
          [ ClassName "alert"
            Src ("img/UI/Warning.png") ]
        label
          [ ClassName "details-label"
            HtmlFor id ]
          [ str "Show Warnings"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id id ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus).Source @
                    [ ul []
                        [ for reason in model.PriceStatusData priceStatus do
                            invalidReason reason ] ] ) ] ]
  | Valid ->
      [ ofInt (fst priceStatuses.Head).Value
        for price in priceStatuses do
          img
            [ ClassName "price-img"
              Src ("img/Sources/" + (fst price).Source.Value + ".png") ] ]
  | Invalid ->
      let id = System.Guid.NewGuid().ToString()
      [ img
          [ ClassName "alert"
            Src ("img/UI/Error.png") ]
        label
          [ ClassName "details-label"
            HtmlFor id ]
          [ str "Show Errors"]
        input
          [ Type "checkbox"
            ClassName "details-input"
            Id id ]
        ul [ ClassName "details" ]
            [ for priceStatus in priceStatuses do
                li []
                  ( sourceIcon (fst priceStatus).Source @
                    [ ul []
                        [ for reason in model.PriceStatusData priceStatus do
                            invalidReason reason ] ] ) ] ]

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
            checkboxText "Ignore Profession Relationships" ToggleIgnoreProfessions model.IgnoreProfessions dispatch ]
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
                      let priceStatuses = model.BestPrices fert.Sources fert.PriceFrom
                      let status = 
                        if (List.exists (fun ps -> snd ps = Valid) priceStatuses) then
                          Valid
                        elif (List.exists (fun ps -> snd ps = Warning) priceStatuses) then
                          Warning
                        else
                          Invalid
                      tr []
                        [ td [] [ statusCheckbox [] (ToggleFertSelected fert) fert.Selected status dispatch ] 
                          td []
                            [ img
                                [ ClassName "fertilizer-img"
                                  Src ("/img/Fertilizers/" + fert.Name + ".png") ]
                              str fert.Name ]
                          td [] [ ofInt fert.Quality ]
                          td [] [ ofFloat fert.Speed ]
                          td [] (viewPrices model priceStatuses) ] ] ] ]
      | Buy ->
          [ buySources model.BuySourceList model.BuySources dispatch ]
      | Sell ->
          [ sellSources model dispatch ]
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