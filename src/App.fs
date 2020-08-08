module StardewValleyStonks.App

open Fable.Core
open Fable.Core.JsInterop
open Elmish

importAll "../sass/main.sass"

open Types

//--Update--
open Browser

type Message =
  | SetPage of Page
  | SetSidebarTab of SidebarTab
  | CloseSidebar
  | ToggleIgnoreProfessionRelationships
  | SetSkillLevel of Skill: NameOf<Skill> * Level: int
  | SetSkillBuff of Skill: NameOf<Skill> * Buff: int
  | ToggleProfession of NameOf<Skill> * NameOf<Profession>
  | ToggleBuySource of NameOf<Source>
  | ToggleMatchCondition of NameOf<MatchCondition>
  | ToggleProcessor of NameOf<Processor>
  | ToggleSellRawCrop
  | ToggleSellSeedsFromSeedMaker
  | ToggleBuySeeds
  | ToggleSeedMakerReplant
  | ToggleSeedOrCropReplant
  | ToggleCropSelected of NameOf<Crop>
  | SetCropSort of CropSort
  | ToggleShowUselessCrops
  | ToggleFertilizerSelected of NameOf<Fertilizer>
  | SetFertilizerSort of FertilizerSort
  | SetStartingFertilizer of NameOf<Fertilizer> option
  | SetStartDay of int
  | SetStartSeason of Season
  | SetEndDay of int
  | SetEndSeason of Season
  | SetYear of int
  | SetYearRequirementsShould of RequirementsShould
  | SetSkillLevelRequirementsShould of RequirementsShould
  | ToggleSpecialCharm
  | SetLuckBuff of int
  | SetGiantCropChecksPerTile of float
  | ToggleGreenhouseMode
  | ToggleQualityProducts
  | TogglePreservesQuality of NameOf<Processor>
  | ToggleQualitySeedMaker
  | SetQualitySeedMakerAmount of Quality * Amount: float
  | Calculate

let update message model =
  match message with
  | SetPage page -> { model with Page = page }
  | SetSidebarTab tab ->
      if tab = model.SidebarTab then
        { model with SidebarOpen = not model.SidebarOpen }
      else
        { model with
            SidebarTab = tab
            SidebarOpen = true }
  | CloseSidebar -> { model with SidebarOpen = false }
  | ToggleIgnoreProfessionRelationships -> { model with IgnoreProfessionRelationships = not model.IgnoreProfessionRelationships }
  | SetSkillLevel (skill, level) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Level = Skill.validLevel level } ) }
  | SetSkillBuff (skill, buff) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Buff = Skill.validBuff buff } ) }
  | ToggleProfession (skill, profession) ->
      { model with Skills = model.Skills.Add(skill, profession |> Profession.toggle model.Skills.[skill] model.IgnoreProfessionRelationships) }
  | ToggleBuySource source -> { model with BuySources = model.BuySources.Add(source, model.BuySources.[source].Toggle) }
  | ToggleMatchCondition cond -> { model with MatchConditions = model.MatchConditions.Add(cond, model.MatchConditions.[cond].Toggle) }
  | ToggleProcessor processor -> { model with Processors = model.Processors.Add(processor, model.Processors.[processor].Toggle) }
  | ToggleSellRawCrop -> { model with SellRawCrop = not model.SellRawCrop }
  | ToggleSellSeedsFromSeedMaker -> { model with SellSeedsFromSeedMaker = not model.SellSeedsFromSeedMaker }
  | ToggleBuySeeds -> { model with BuySeeds = not model.BuySeeds }
  | ToggleSeedMakerReplant -> { model with SeedMakerReplant = not model.SeedMakerReplant }
  | ToggleSeedOrCropReplant -> { model with SeedOrCropReplant = not model.SeedOrCropReplant }
  | ToggleCropSelected crop -> { model with Crops = model.Crops.Add(crop, model.Crops.[crop].Toggle) }
  | SetCropSort sort ->
      if sort = model.CropSort then
        { model with CropSortAscending = not model.CropSortAscending }
      else
        { model with
            CropSort = sort
            CropSortAscending = true }
  | ToggleShowUselessCrops -> { model with ShowUselessCrops = not model.ShowUselessCrops }
  | ToggleFertilizerSelected fert -> { model with Fertilizers = model.Fertilizers.Add(fert, model.Fertilizers.[fert].Toggle) }
  | SetFertilizerSort sort ->
      if sort = model.FertilizerSort then
        { model with FertilizerSortAscending = not model.FertilizerSortAscending }
      else
        { model with
            FertilizerSort = sort
            FertilizerSortAscending = true }
  | SetStartingFertilizer fert -> { model with StartingFertilizer = fert }
  | SetStartDay day -> { model with StartDate = { model.StartDate with Day = Date.validDay day } }
  | SetStartSeason season -> { model with StartDate = { model.StartDate with Season = season } }
  | SetEndDay day -> { model with EndDate = { model.EndDate with Day = Date.validDay day } }
  | SetEndSeason season -> { model with EndDate = { model.EndDate with Season = season } }
  | SetYear year -> { model with Year = year }
  | SetYearRequirementsShould mode -> { model with YearRequirementsShould = mode }
  | SetSkillLevelRequirementsShould mode -> { model with SkillLevelRequirementsShould = mode }
  | ToggleSpecialCharm -> { model with SpecialCharm = not model.SpecialCharm }
  | SetLuckBuff buff -> { model with LuckBuff = positive buff }
  | SetGiantCropChecksPerTile checks -> { model with GiantCropChecksPerTile = checks |> clamp 0.0 9.0 }
  | ToggleGreenhouseMode -> { model with GreenhouseMode = not model.GreenhouseMode }
  | ToggleQualityProducts -> { model with QualityProducts = not model.QualityProducts }
  | TogglePreservesQuality processor -> { model with Processors = model.Processors.Add(processor, model.Processors.[processor].TogglePreservesQuality) }
  | ToggleQualitySeedMaker -> { model with QualitySeedMaker = not model.QualitySeedMaker }
  | SetQualitySeedMakerAmount (quality, amount) -> { model with QualitySeedMakerAmounts = model.QualitySeedMakerAmounts.Add(quality, positivef amount) }
  | Calculate -> model //Model.calculate model

//--View--
open Fable.React
open Fable.React.Props
open Elmish.React.Common
open Elmish.React.Helpers

let classModifier baseClass modifier apply =
  ClassName <| if apply then baseClass + "--" + modifier else baseClass

let checkboxWith alsoDisplay (message: Message) isChecked dispatch =
  label [ ClassName "checkbox-img-label" ]
    ( [ input
          [ Type "checkbox"
            Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
            Checked isChecked
            OnChange <| fun _ -> dispatch message ]
        img
          [ ClassName "checkbox-img"
            Src <| if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png" ] ]
      @ alsoDisplay)

let checkbox = checkboxWith []

let checkboxWithText text = checkboxWith [ str text ]

let checkboxImg isChecked status =
  if isChecked then
    match status with
    | Valid -> "img/UI/CheckboxGreen.png"
    | Warning -> "img/UI/CheckboxYellow.png"
    | Invalid -> "img/UI/CheckboxRed.png"
  else
    "img/UI/Checkbox.png"

let statusCheckboxWith alsoDisplay (message: Message) isChecked status dispatch =
  label [ ClassName "checkbox-img-label" ]
    ( [ input
          [ Type "checkbox"
            Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
            Checked isChecked
            OnChange <| fun _ -> dispatch message ]
        img
          [ ClassName "checkbox-img"
            Src <| checkboxImg isChecked status ] ]
      @ alsoDisplay)

let statusCheckbox = statusCheckboxWith []

let viewTab toString css message tab active dispatch =
  li
    [ classModifier (css + "-tab") "active" active
      OnClick <| fun _ -> dispatch <| message tab ]
    [ str <| toString tab ]

let viewTabsWith activeFun toString css message list dispatch =
  ul [ ClassName <| css + "-tabs" ]
    [ for tab in list do
        viewTab toString css message tab (activeFun tab) dispatch ]

let viewTabs css message list currentTab dispatch =
  viewTabsWith ((=) currentTab) css message list dispatch

let warningIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Warning.png" ]

let errorIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Error.png" ]

let levelInput mode name level dispatch =
  input
    [ Type mode
      Min 0
      Max 10
      valueOrDefault level
      ClassName <| "skill-" + mode + "-input"
      OnChange <| fun l -> dispatch <| SetSkillLevel (name, !!l.Value) ]

let skillLevelInput name level dispatch =
  label [ ClassName "skill-input" ]
    [ str "Level: "
      levelInput "range" name level dispatch
      levelInput "number" name level dispatch ]

let skillBuffInput name buff dispatch =
  label [ ClassName "skill-input" ]
    [ str "Buff: "
      input
        [ Type "number"
          Min 0
          valueOrDefault buff
          ClassName "skill-number-input"
          OnChange <| fun b -> dispatch <| SetSkillBuff (name, !!b.Value) ] ]

let profession requirementsShould profession skill dispatch =
  button
    [ classModifier
        "profession"
        ( if profession |> Profession.isUnlocked skill || requirementsShould <> Invalidate then
            "active"
          else
            "error")
        skill.Professions.[profession].Selected
      OnClick <| fun _ -> dispatch <| ToggleProfession (Types.Name skill.Name, profession) ]
    [ if skill.Professions.[profession].Selected && not (profession |> Profession.isUnlocked skill) then
        if requirementsShould = Warn then
          warningIcon
        elif requirementsShould = Invalidate then
          errorIcon
      img
        [ ClassName "profession-img"
          Src <| "img/Skills/" + ofName profession + ".png" ]
      str <| ofName profession ]

let professionRow requirementsShould skill row dispatch =
  div [ ClassName "profession-row" ]
    [ for name in row do
        profession requirementsShould name skill dispatch ]

let viewProfessions requirementsShould skill dispatch =
  div [ ClassName "professions" ]
    [ for row in skill.ProfessionLayout do
        professionRow requirementsShould skill row dispatch ]

let sourceIcon name =
  [ img
      [ ClassName "source-img"
        Src <| "img/Sources/" + name + ".png" ]
    str name ]

let buySource name selected dispatch =
  li []
    [ checkboxWith (sourceIcon <| ofName name) (ToggleBuySource name) selected dispatch ]

let buySources list (sources: Map<NameOf<Source>, Source>) dispatch =
  ul [ ClassName "source-list" ]
    [ for name in list do
        buySource name sources.[name].Selected dispatch ]

let rec viewAlert = function
  | UnmetRequirement c ->
      match c with
      | SkillLevel (skill, level) -> [ str <| ofName skill + " level too low. Unlocks at level " + string level + "." ]
      | Year y -> [ str <| "Available only from year " + string y + " and onwards." ]
  | Message message -> [ str message ]
  | AlertList (a, list) ->
      [ str a
        ul [ ClassName "alert-list" ]
          [ for subAlert in list do
              li []
                (viewAlert subAlert) ] ]

let viewAlertOption message alert =
  match alert with
  | [] -> None
  | alerts -> AlertList (message, alerts) |> viewAlert |> div [] |> Some
  |> ofOption

let viewAlerts name (warning, error) =
  match warning, error with
  | [], [] -> None
  | warnings, errors ->
      div []
        [ label
            [ ClassName "alerts-label"
              HtmlFor name ]
            [ str "Show Alerts"]
          input
            [ Type "checkbox"
              ClassName "alerts-toggle"
              Id name ]
          div [ ClassName "alerts" ]
            [ viewAlertOption "Warning:" warnings
              viewAlertOption "Error:" errors ] ]
      |> Some
  |> ofOption

let processor model name dispatch =
  li []
    [ checkboxWith (sourceIcon <| ofName name) (ToggleProcessor name) model.Processors.[name].Selected dispatch
      viewAlerts (ofName name) (Model.processorAlert model name) ]

let replants model dispatch =
  ul [ ClassName "source-list" ]
    [ li []
        [ checkboxWith (sourceIcon "Buy Seeds") ToggleBuySeeds model.BuySeeds dispatch ]
      li []
        [ checkboxWith (sourceIcon "Seed Maker") ToggleSeedMakerReplant model.SeedMakerReplant dispatch
          viewAlerts "SeedMakerReplant" (Model.seedMakerAlert model.SeedMakerReplant model) ]
      li []
        [ checkboxWith (sourceIcon "Harvested Seed or Crop") ToggleSeedOrCropReplant model.SeedOrCropReplant dispatch ] ]

let selectList (list: 't seq) toString (ofString: string -> 't) text message (value: 't) dispatch =
  label []
      [ ofOption <| Option.bind (fun t -> Some <| str (t + ": ")) text
        select
          [ valueOrDefault <| toString value
            OnChange <| fun x -> dispatch <| (message <| ofString x.Value) ]
          [ for o in list do
              option [ Value <| (toString o :> obj) ]
                [ str <| toString o ] ] ]

let selectRequirementsShould = selectList RequirementsShould.all string RequirementsShould.parse

let test (ofString: string -> 't) = stringToOption ofString

let selectListOption (list: 't list) toString (ofString: string -> 't) =
  selectList (None::(List.map Some list)) (optionToString toString) (stringToOption ofString)

let viewPrices model priceFrom =
  match Model.priceData model priceFrom with
  | PriceData (price, sources) ->
      [ str <| string price + "g"
        for source, status in sources do
          img
            [ classModifier "price-img" "warning" (status = Warning)
              Src <| "img/Sources/" + ofName source + ".png" ] ]
  | NoValidPrices ->
      [ errorIcon
        str "No valid prices." ]
  | NoPrices -> [ str "N/A" ]

let selectSeasons = selectList Season.all string Season.parse None

let dayInput message day dispatch =
  label [ ClassName "day-input" ]
    [ input
        [ Type "number"
          Min 1
          Max 28
          valueOrDefault day
          ClassName "day-number-input"
          OnChange <| fun b -> dispatch <| message !!b.Value ] ]

let date text seasonMsg dayMsg date dispatch =
  div [ ClassName "date" ]
    [ str text
      selectSeasons seasonMsg date.Season dispatch
      dayInput dayMsg date.Day dispatch ]

let tableItems
  toKey
  priceFrom
  selectedMessage
  (toNameOf: 't -> NameOf<'t>)
  selected
  status
  imgPath
  (properties: 't -> ReactElement list list)
  (items: 't list)
  model
  dispatch =
  [ for item in items do
      tr
        [ ClassName "table-row"
          Key <| toKey item ]
        [ td []
            [ statusCheckbox (selectedMessage <| toNameOf item) (selected item) (status model item) dispatch ]
          td []
            [ img
                [ ClassName "table-item-img"
                  Src <| imgPath item ]
              str <| toKey item ]
          for property in properties item do
            td []
              property
          td []
            (viewPrices model (priceFrom item)) ] ]
  |> ofList

let cropTableItems =
  tableItems
    Crop.name
    Crop.priceFrom
    ToggleCropSelected
    Crop.nameOf
    Crop.selected
    Model.cropStatus
    (fun crop -> "img/Crops/" + Crop.name crop + ".png")
    (fun crop ->
      [ [ ofInt <| Crop.totalGrowthTime crop ]
        [ ofOption <| Option.bind (ofInt >> Some) (Crop.regrowTime crop) ]
        [ for season in Crop.selectedSeasons crop do
            str <| string season
            br [] ] ] )

let fertilizerTableItems =
  tableItems
    Fertilizer.name
    Fertilizer.priceFrom
    ToggleFertilizerSelected
    Fertilizer.nameOf
    Fertilizer.selected
    Model.fertilizerStatus
    (fun fert -> "img/Fertilizers/" + fert.Name + ".png")
    (fun fert ->
      [ [ ofInt fert.Quality ]
        [ str <| string (fert.Speed * 100.0) + "%" ] ] )

let viewTable columns sortMessage data dispatch =
  [ table [ ClassName "table-header" ]
      [ colgroup []
          [ for _, width, _ in columns do
              col [ Style [ Width (string width + "%") ] ] ]
        thead []
          [ tr []
              [ for name, width, sort in columns do
                  th
                    [ OnClick (fun _ -> dispatch <| sortMessage sort)
                      Style [ Width (string width + "%") ] ]
                    [ str name ] ] ] ]
    div [ ClassName "table-body-container" ]
      [ table [ ClassName "table" ]
          [ colgroup []
              [ for _, width, _ in columns do
                  col [ Style [ Width (string width + "%") ] ] ]
            tbody []
              [ data ] ] ] ]

let sidebarContent model dispatch =
  match model.SidebarTab with
  | Skills ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ ul [ ClassName "skills" ]
            [ for skill in model.SkillList do
                li [ ClassName "skill" ]
                  [ span [ ClassName "skill-header" ]
                      [ img
                          [ ClassName "skill-img"
                            Src <| "img/Skills/" + ofName skill + ".png" ]
                        str model.Skills.[skill].Name ]
                    skillLevelInput skill model.Skills.[skill].Level dispatch
                    skillBuffInput skill model.Skills.[skill].Buff dispatch
                    viewProfessions model.SkillLevelRequirementsShould model.Skills.[skill] dispatch ] ]
          checkboxWithText "Ignore Profession Relationships" ToggleIgnoreProfessionRelationships model.IgnoreProfessionRelationships dispatch ]
  | Crops ->
      div [ classModifier "sidebar-table-content" "open" model.SidebarOpen ]
        [ yield! viewTable
            [ "", 5.0, CropSort.Selected
              "Crop", 40.0, CropSort.ByName
              "Growth Time", 7.5, TotalGrowthTime
              "Regrow Time", 7.5, RegrowTime
              "Seasons", 15.0, Seasons
              "Seed Price", 25.0, SeedPrice ]
            SetCropSort
            (cropTableItems (Model.sortedCrops model) model dispatch)
            dispatch
          checkboxWithText "Show Crops That Cannot Give One Harvest" ToggleShowUselessCrops model.ShowUselessCrops dispatch ]
  | Fertilizers ->
      div [ classModifier "sidebar-table-content" "open" model.SidebarOpen ]
        [ yield! viewTable
            [ "", 5.0, FertilizerSort.Selected
              "Fertilizer", 50.0, FertilizerSort.ByName
              "Quality", 10.0, Quality
              "Speed Bonus", 10.0, Speed
              "Price", 25.0, Price ]
            SetFertilizerSort
            (fertilizerTableItems (Model.sortedFertilizers model) model dispatch)
            dispatch
          selectListOption
            model.FertilizerList
            ofName
            Types.Name
            (Some "Starting Fertilizer: ")
            SetStartingFertilizer
            model.StartingFertilizer
            dispatch ]
  | Buy ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ buySources model.BuySourceList model.BuySources dispatch
          ul [ ClassName "match-condition-list" ]
            [ for cond in model.MatchConditionList do
              checkboxWithText (ofName cond) (ToggleMatchCondition cond) model.MatchConditions.[cond].Selected dispatch ]
          replants model dispatch ]
  | Sell ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ ul [ ClassName "source-list" ]
            [ li []
                [ checkboxWith (sourceIcon "Raw Crop") ToggleSellRawCrop model.SellRawCrop dispatch ]
              for name in model.ProcessorList do
                processor model name dispatch
              li []
                [ checkboxWith (sourceIcon "Seed Maker") ToggleSellSeedsFromSeedMaker model.SellSeedsFromSeedMaker dispatch
                  viewAlerts "SellSeedsFromSeedMaker" (Model.seedMakerAlert model.SellSeedsFromSeedMaker model) ] ] ]
  | Date ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ date "Start Date: " SetStartSeason SetStartDay model.StartDate dispatch
          date "End Date: " SetEndSeason SetEndDay model.EndDate dispatch
          if not <| Model.validDate model then
            span []
              [ errorIcon
                if model.StartDate = model.EndDate then
                  str "The end date cannot be the same as the start date."
                else
                  str "The end date cannot be before the start date." ]
          label [ ClassName "year-input" ]
            [ str "Year: "
              input
                [ Type "number"
                  Min 1
                  valueOrDefault model.Year
                  ClassName "year-number-input"
                  OnChange (fun y -> dispatch <| SetYear !!y.Value) ] ] ]
  | Settings ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ selectRequirementsShould (Some "Year Requirements") SetYearRequirementsShould model.YearRequirementsShould dispatch
          selectRequirementsShould (Some "Skill Level Requirements") SetSkillLevelRequirementsShould model.SkillLevelRequirementsShould dispatch
          checkboxWithText "Special Charm" ToggleSpecialCharm model.SpecialCharm dispatch
          label [ ClassName "setting-input" ]
            [ str "Luck Buff: "
              input
                [ Type "number"
                  Min 0
                  valueOrDefault model.LuckBuff
                  ClassName "setting-number-input"
                  OnChange (fun b -> dispatch <| SetLuckBuff !!b.Value) ] ]
          label [ ClassName "setting-input" ]
            [ str "Giant Crop Checks Per Tile: "
              input
                [ Type "number"
                  Min 0.0
                  Max 9.0
                  Step "any"
                  valueOrDefault model.GiantCropChecksPerTile
                  ClassName "setting-number-input"
                  OnChange (fun c -> dispatch <| SetGiantCropChecksPerTile !!c.Value) ] ]
          checkboxWithText "Greenhouse Mode" ToggleGreenhouseMode model.GreenhouseMode dispatch ]
  | Mod ->
      div [ classModifier "sidebar-content" "open" model.SidebarOpen ]
        [ checkboxWithText "Quality Products" ToggleQualityProducts model.QualityProducts dispatch
          ul []
            [ for processor in model.ProcessorList do
                li []
                  [ //visual indicator for disabled
                    label
                      [ ClassName "checkbox-img-label" ]
                      ( [ input
                            [ Type "checkbox"
                              Disabled <| not model.QualityProducts
                              Style [ Visibility "hidden"; Position PositionOptions.Absolute ]
                              Checked model.Processors.[processor].PreservesQuality
                              OnChange (fun _ -> dispatch <| TogglePreservesQuality processor) ]
                          img
                            [ ClassName "checkbox-img"
                              Src (if model.Processors.[processor].PreservesQuality then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png") ] ]
                        @ (sourceIcon <| ofName processor)) ] ]
          checkboxWithText "Quality Seed Maker" ToggleQualitySeedMaker model.QualitySeedMaker dispatch
          str "(Average) Seed Amounts: "
          ul []
            [ for KeyValue(quality, amount) in model.QualitySeedMakerAmounts do
                li []
                  [ label []
                      [ str <| string quality + ": "
                        input
                          [ Type "number"
                            Min 0
                            Disabled <| not model.QualitySeedMaker
                            valueOrDefault amount
                            ClassName "skill-number-input"
                            OnChange (fun v -> dispatch <| SetQualitySeedMakerAmount (quality, !!v.Value)) ] ] ] ] ]
  //lazyView2With (fun oldModel newModel -> (not oldModel.SidebarOpen && not newModel.SidebarOpen) || oldModel = newModel) sidebarContent

let cover sidebarOpen dispatch =
  div
    [ classModifier "cover" "open" sidebarOpen
      OnClick (fun _ -> dispatch CloseSidebar) ]
    []

let sidebar model dispatch =
  div [ classModifier "sidebar" "open" model.SidebarOpen ]
    [ sidebarContent model dispatch
      viewTabsWith (fun t -> model.SidebarOpen && t = model.SidebarTab) string "sidebar" SetSidebarTab SidebarTab.all dispatch ]

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
          button [ OnClick (fun _ -> dispatch Calculate) ]
            [ str "Calculate" ]
          cover model.SidebarOpen dispatch
          sidebar model dispatch ]

//--App--
open Elmish.React
open Elmish.Debug
open Elmish.HMR

Program.mkSimple (fun _ -> Model.initial) update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run