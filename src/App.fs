module StardewValleyStonks.App

open Fable.Core.JsInterop
open Elmish

importAll "../sass/main.sass"

open Types

//--Update--

type Message =
  | SetPage of Page
  | SetSidebarTab of SidebarTab
  | CloseSidebar

  | SetSkillLevel of Skills * int
  | SetSkillBuff of Skills * int
  | ToggleProfession of Skills * NameOf<Profession>
  | ToggleIgnoreProfessionRelationships

  | ToggleBuySource of NameOf<Source>
  | ToggleRelativePrice of NameOf<PriceMultiplier>

  | ToggleProcessor of NameOf<Processor>
  | ToggleSellRawCrop
  | ToggleSellSeedsFromSeedMaker

  | ToggleBuySeeds
  | ToggleSeedMakerReplant
  | ToggleHarvestReplant

  | ToggleCropSelected of NameOf<Crop>
  | SetCropSort of CropSort
  | SetSelectedCrop of NameOf<Crop> option
  | SetCropTab of CropTab
  | ToggleShowOutOfSeasonCrops
  | ToggleShowInvalidCrops
  | ToggleAllowCropClearings
  | ToggleAllowCrossSeason
  | ToggleAccountForReplant

  | ToggleFertilizerSelected of NameOf<Fertilizer>
  | SetFertilizerSort of FertilizerSort
  | SetSelectedFertilizer of NameOf<Fertilizer> option
  | ToggleAccountForFertilizerCost

  | SetStartDay of int
  | SetStartSeason of Season
  | SetEndDay of int
  | SetEndSeason of Season

  | SetCompareMode of CompareMode
  | ToggleShowUnprofitableCombos
  | SetSelectedCombo of (NameOf<Crop> * NameOf<Fertilizer> option) option

  | SetSelectedCompareCrop of NameOf<Crop> option
  | SetCompareCropsUsingFertilizer of NameOf<Fertilizer> option

  | SetSelectedCompareFertilizer of NameOf<Fertilizer> option option
  | SetCompareFertilizersUsingCrop of NameOf<Crop>

  | SetStartingFertilizer of NameOf<Fertilizer> option

  | SetProfitMode of ProfitMode
  | ToggleGreenhouse

  | ToggleShowTips
  | ToggleSaveSettings
  | SetSkillLevelPolicy of RequirementPolicy

  | ToggleSpecialCharm
  | SetLuckBuff of int

  | SetGiantCropChecksPerTile of float

  | ToggleQualityProducts
  | TogglePreservesQuality of NameOf<Processor>
  | ToggleQualitySeedMaker
  | SetQualitySeedMakerAmount of Quality * Amount: float
  | Calculate

open Elmish.Navigation

let urlUpdate page model =
  match page with
  | Some x -> { model with Page = x }, []
  | None -> model, Navigation.modifyUrl (Page.url model.Page)

let update message model =
  match message with
  | SetPage page -> { model with Page = page }, Navigation.newUrl (Page.url page)
  | SetSidebarTab tab ->
      if tab = model.SidebarTab then
        { model with SidebarOpen = not model.SidebarOpen }, []
      else
        { model with
            SidebarTab = tab
            SidebarOpen = true }, []
  | CloseSidebar -> { model with SidebarOpen = false }, []

  | SetSkillLevel (skill, level) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Level = Skill.validLevel level } ) }, []
  | SetSkillBuff (skill, buff) ->
      { model with Skills = model.Skills.Add(skill, { model.Skills.[skill] with Buff = Skill.validBuff buff } ) }, []
  | ToggleProfession (skill, profession) ->
      { model with Skills = model.Skills.Add(skill, profession |> Profession.toggle model.Skills.[skill] model.IgnoreProfessionRelationships) }, []
  | ToggleIgnoreProfessionRelationships -> { model with IgnoreProfessionRelationships = not model.IgnoreProfessionRelationships }, []
  
  | ToggleBuySource source -> { model with BuySources = model.BuySources.Add(source, model.BuySources.[source].Toggle) }, []
  | ToggleRelativePrice price -> { model with PriceMultipliers = model.PriceMultipliers.Add(price, model.PriceMultipliers.[price].Toggle) }, []

  | ToggleProcessor processor -> { model with Processors = model.Processors.Add(processor, model.Processors.[processor].Toggle) }, []
  | ToggleSellRawCrop -> { model with SellRawCrop = not model.SellRawCrop }, []
  | ToggleSellSeedsFromSeedMaker -> { model with SellSeedsFromSeedMaker = not model.SellSeedsFromSeedMaker }, []

  | ToggleBuySeeds -> { model with BuySeeds = not model.BuySeeds }, []
  | ToggleSeedMakerReplant -> { model with SeedMakerReplant = not model.SeedMakerReplant }, []
  | ToggleHarvestReplant -> { model with HarvestReplant = not model.HarvestReplant }, []

  | ToggleCropSelected crop -> { model with Crops = model.Crops.Add(crop, Crop.toggle model.Crops.[crop]) }, []
  | SetCropSort sort ->
      if sort = model.CropSort then
        { model with CropSortAscending = not model.CropSortAscending }, []
      else
        { model with
            CropSort = sort
            CropSortAscending = true }, []
  | SetSelectedCrop crop -> { model with SelectedCrop = crop }, []
  | SetCropTab tab -> { model with CropTab = tab }, []
  | ToggleShowOutOfSeasonCrops -> { model with ShowOutOfSeasonCrops = not model.ShowOutOfSeasonCrops }, []
  | ToggleShowInvalidCrops -> { model with ShowInvalidCrops = not model.ShowInvalidCrops }, []
  | ToggleAllowCropClearings -> { model with AllowCropClearings = not model.AllowCropClearings }, []
  | ToggleAllowCrossSeason -> { model with AllowCrossSeason = not model.AllowCrossSeason }, []
  | ToggleAccountForReplant -> { model with AccountForReplant = not model.AccountForReplant }, []

  | ToggleFertilizerSelected fert -> { model with Fertilizers = model.Fertilizers.Add(fert, model.Fertilizers.[fert].Toggle) }, []
  | SetFertilizerSort sort ->
      if sort = model.FertilizerSort then
        { model with FertilizerSortAscending = not model.FertilizerSortAscending }, []
      else
        { model with
            FertilizerSort = sort
            FertilizerSortAscending = true }, []
  | SetSelectedFertilizer fert -> { model with SelectedFertilizer = fert }, []
  | ToggleAccountForFertilizerCost -> { model with AccountForFertilizerCost = not model.AccountForFertilizerCost }, []

  | SetStartDay day -> { model with StartDate = { model.StartDate with Day = Date.validDay day } }, []
  | SetStartSeason season -> { model with StartDate = { model.StartDate with Season = season } }, []
  | SetEndDay day -> { model with EndDate = { model.EndDate with Day = Date.validDay day } }, []
  | SetEndSeason season -> { model with EndDate = { model.EndDate with Season = season } }, []

  | SetCompareMode mode -> { model with CompareMode = mode }, []
  | ToggleShowUnprofitableCombos -> { model with ShowUnprofitableCombos = not model.ShowUnprofitableCombos }, []
  | SetSelectedCombo combo -> { model with SelectedCombo = combo }, []

  | SetSelectedCompareCrop crop -> { model with SelectedCompareCrop = crop }, []
  | SetCompareCropsUsingFertilizer fert -> { model with CompareCropsUsingFertilizer = fert }, []

  | SetSelectedCompareFertilizer fert -> { model with SelectedCompareFertilizer = fert }, []
  | SetCompareFertilizersUsingCrop crop -> { model with CompareFertilizersUsingCrop = crop }, []

  | SetStartingFertilizer fert -> { model with StartingFertilizer = fert }, []

  | SetProfitMode mode -> { model with ProfitMode = mode }, []
  | ToggleGreenhouse -> { model with Greenhouse = not model.Greenhouse }, []

  | ToggleShowTips -> { model with ShowTips = not model.ShowTips }, []
  | ToggleSaveSettings -> { model with SaveSettings = not model.SaveSettings }, []
  | SetSkillLevelPolicy policy -> { model with SkillLevelPolicy = policy }, []
  
  | ToggleSpecialCharm -> { model with SpecialCharm = not model.SpecialCharm }, []
  | SetLuckBuff buff -> { model with LuckBuff = positive buff }, []
  
  | SetGiantCropChecksPerTile checks -> { model with GiantCropChecksPerTile = checks |> clamp 0.0 9.0 }, []

  | ToggleQualityProducts -> { model with QualityProducts = not model.QualityProducts }, []
  | TogglePreservesQuality processor -> { model with Processors = model.Processors.Add(processor, model.Processors.[processor].TogglePreservesQuality) }, []
  | ToggleQualitySeedMaker -> { model with QualitySeedMaker = not model.QualitySeedMaker }, []
  | SetQualitySeedMakerAmount (quality, amount) -> { model with QualitySeedMakerAmounts = model.QualitySeedMakerAmounts.Add(quality, positivef amount) }, []
  | Calculate -> Model.calculate model, []

//--View--
open Fable.React
open Fable.React.Props
open Elmish.React.Helpers

let gold (price: int) = string price + "g"
let percent value = string (value * 100.0) + "%"
let percent2 value = sprintf "%.2f%%" (value * 100.0)

let strOption text = ofOption <| Option.bind (str >> Some) text

let classModifier baseClass modifier apply =
  ClassName <| if apply then baseClass + "--" + modifier else baseClass

let classBase baseClass single add = classBaseList baseClass [ single, add ]

let checkboxCss css alsoDisplay (message: Message) isChecked dispatch =
  label [ css ]
    ( [ input
          [ Type "checkbox"
            Checked isChecked
            OnChange <| fun _ -> dispatch message ]
        img
          [ ClassName "checkbox-img"
            Src <| if isChecked then "img/UI/CheckboxGreen.png" else "img/UI/Checkbox.png" ] ]
      @ alsoDisplay)

let checkboxWith = checkboxCss (ClassName "checkbox-label")

let checkboxDisabledWith alsoDisplay disabled =
  checkboxCss (classBase "checkbox-label" "disabled" disabled) alsoDisplay

let checkboxDisabled = checkboxDisabledWith []

let checkboxDisabledText text = checkboxDisabledWith [ str text ]

let checkbox = checkboxWith []

let checkboxText text = checkboxWith [ str text ]

let viewTab toString message tab active dispatch =
  li
    [ if active then ClassName "active"
      OnClick <| fun _ -> dispatch <| message tab ]
    [ str <| toString tab ]

let viewTabsWith toString css message list activeFun dispatch =
  ul [ ClassName css ]
    [ for tab in list do
        viewTab toString message tab (activeFun tab) dispatch ]

let inline viewTabs css message list currentTab =
  viewTabsWith string css message list ((=) currentTab)

let warningIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Warning.png" ]

let errorIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Error.png" ]

let skillLevelInput name level dispatch =
  label []
    [ str "Level:"
      input
        [ Type "range"
          Min 0
          Max 10
          valueOrDefault level
          OnChange <| fun l -> dispatch <| SetSkillLevel (name, !!l.Value) ]
      input
        [ Type "number"
          Min 0
          Max 10
          valueOrDefault level
          OnChange <| fun l -> dispatch <| SetSkillLevel (name, !!l.Value) ] ]

let skillBuffInput name buff dispatch =
  label []
    [ str "Buff:"
      input
        [ Type "number"
          Min 0
          valueOrDefault buff
          OnChange <| fun b -> dispatch <| SetSkillBuff (name, !!b.Value) ] ]

let profession requirementsShould profession skill dispatch =
  let selected = skill.Professions.[profession].Selected
  let unlocked = profession |> Profession.isUnlocked skill
  button
    [ classList
        [ "active", selected
          "disabled", not unlocked && requirementsShould = Enforce ]
      OnClick <| fun _ -> dispatch <| ToggleProfession (Skill.which skill, profession) ]
    [ if selected && not unlocked && requirementsShould = Warn then
        warningIcon
      img [ Src <| "img/Skills/" + ofName profession + ".png" ]
      str <| ofName profession ]

let professionTree requirementsShould tree skill dispatch =
  div []
    [ div []
        [ profession requirementsShould tree.Level5Profession skill dispatch ]
      match tree.Level10Profession with
      | NoLevel10 -> nothing
      | Single p ->
          div []
            [ profession requirementsShould p skill dispatch ]
      | Pair (l, r) ->
          div []
            [ profession requirementsShould l skill dispatch
              profession requirementsShould r skill dispatch ] ]

let viewProfessions requirementsShould skill dispatch =
  div [ ClassName "professions" ]
    [ for tree in skill.ProfessionTrees do
        professionTree requirementsShould tree skill dispatch ]

let viewDistribution (dist: Map<_,_>) =
  div [ ClassName "quality-distribution" ]
    [ for KeyValue(quality, prob) in dist do
        div
          [ Style
              [ Width (percent prob)
                BackgroundColor (Quality.color quality) ] ]
          [ str <| string quality + ": " + percent2 prob ] ]

let viewSkillDistribution = Model.distribution >>| viewDistribution

let buyAndSellIcon name =
  [ img
      [ ClassName "source-img"
        Src <| "img/BuyAndSell/" + name + ".png" ]
    str name ]

let buySource name selected dispatch =
  li []
    [ checkboxWith (buyAndSellIcon <| ofName name) (ToggleBuySource name) selected dispatch ]

let buySources list (sources: Map<NameOf<Source>, Source>) dispatch =
  ul [ ClassName "source-list" ]
    [ for name in list do
        buySource name sources.[name].Selected dispatch ]

let processor model name dispatch =
  li []
    [ checkboxWith (buyAndSellIcon <| ofName name) (ToggleProcessor name) model.Processors.[name].Selected dispatch
      ]
      //viewAlerts (ofName name) (Model.processorAlert model name) ]

let replants model dispatch =
  ul [ ClassName "source-list" ]
    [ li []
        [ checkboxWith (buyAndSellIcon "Buy Seeds") ToggleBuySeeds model.BuySeeds dispatch ]
      li []
        [ checkboxWith (buyAndSellIcon "Seed Maker") ToggleSeedMakerReplant model.SeedMakerReplant dispatch
          ]
          //viewAlerts "SeedMakerReplant" (Model.seedMakerAlert model model.SeedMakerReplant) ]
      li []
        [ checkboxWith (buyAndSellIcon "Harvested Seed or Crop") ToggleHarvestReplant model.HarvestReplant dispatch ] ]


let selectBase toValue (toString: 't -> _) parse list text (message: 't -> _) (value: 't) dispatch =
  label []
    [ strOption text
      select
        [ valueOrDefault <| toValue value
          OnChange <| fun x -> dispatch <| message (parse x.Value) ]
        [ for item in list do
            option [ Value (toValue item) ]
              [ str <| toString item ] ] ]

let selectWith toString = selectBase toString toString

// For types that do not erase into strings
let inline selectParse parse = selectWith string parse

// For types that erase into strings (i.e. [<StringEnum>] and [<Erase>] on DU's)
let inline selectString list = selectParse (!!) list

let inline selectStringOptionWith none list = selectWith (optionToStringWith none string) (stringToOptionWith none (!!)) (listWithNone list)

let inline selectStringOption list = selectWith (optionToString string) (stringToOption (!!)) (listWithNone list)

let selectFertilizerOption = selectStringOptionWith Fertilizer.none

let viewPrice model (priceFrom: Map<_,_>) source price =
  li [ if not <| Model.priceActive model source price then ClassName "disabled" ]
    [ img
        [ ClassName "price-img"
          Src <| "img/BuyAndSell/" + Buy.sourceName price + ".png" ]
      match price with
      | BuyPrice p -> str <| gold p.Value
      | RelativePrice r ->
          let multiplier = model.PriceMultipliers.[r.Multiplier]
          let a, b =
            let basePrice = Model.priceValue model priceFrom r.RelativeTo
            (basePrice, basePrice |> apply multiplier.Value)
            |> conditional swap (not multiplier.MultiplyWhenSelected)
          if multiplier.Selected then
            span [ Style [ TextDecoration "line-through" ] ]
              [ str <| gold a ]
            span []
              [ str <| gold b ]
          else
            span []
              [ str <| gold a ] ]

let viewPrices model (priceFrom: Map<_,_>) =
  ul []
    [ for KeyValue(source, price) in priceFrom do
        viewPrice model priceFrom source price ]

let viewPriceIcons model (priceFrom: Map<_,_>) =
  if priceFrom.IsEmpty then
    [ str "N/A" ]
  else
    match Model.priceData model priceFrom with
    | Some (bestPrice, sources) ->
        [ str <| gold bestPrice
          for source in sources do
            img
              [ ClassName "price-img"
                Src <| "img/BuyAndSell/" + ofName source + ".png" ] ]
    | None ->
        [ errorIcon
          str "No valid prices." ]

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
    [ str <| text + ":"
      selectBase int Season.name ((!!) >> enum<Season>) Season.all None seasonMsg date.Season dispatch
      dayInput dayMsg date.Day dispatch ]

let viewTable tableCss columns toKey disabled items =
  div [ ClassName tableCss ]
    [ let colGroup =
        colgroup []
          [ for width, _,_ in columns do
              col [ Style [ Width (percent width) ] ] ]

      table [ ClassName "table-header" ]
        [ colGroup
          thead []
            [ tr []
                [ for _, header, _ in columns do
                    yield header ] ] ]

      div [ ClassName "table-body-container" ]
        [ table [ ClassName "table-body" ]
            [ colGroup
              tbody []
                [ ofList
                    [ for item in items do
                        tr
                          [ if disabled item then ClassName "disabled"
                            Key <| toKey item ]
                          [ for _,_, dataCell in columns do
                              yield dataCell item ] ] ] ] ] ]

let tableHeaderCell sortMsg dispatch header sort =
  th [ OnClick <| fun _ -> dispatch <| sortMsg sort ] header

let tableCell select dispatch property item =
  td [ OnClick <| fun _ -> dispatch <| select item ]
    (property item)

let tableImage imgPath name item =
  [ img
      [ ClassName "table-item-img"
        Src <| imgPath item ]
    str <| name item ]

let tableCheckbox selectedMsg toNameOf selected dispatch item =
  [ checkbox (selectedMsg <| toNameOf item) (selected item) dispatch ]

let viewGrowthMultiplier imgPath active name =
  div [ if not active then ClassName "disabled" ]
    [ img [ Src imgPath ]
      str <| ofName name ]

let sidebarContent model dispatch =
  [ match model.SidebarTab with
    | Skills ->
        div [ classBase "sidebar-content" "open" model.SidebarOpen ]
          [ ul [ ClassName "skills" ]
              [ for skill in Skills.all do
                  li []
                    [ span []
                        [ img
                            [ Src <| "img/Skills/" + string skill + ".png" ]
                          str <| string skill ]
                      skillLevelInput skill model.Skills.[skill].Level dispatch
                      skillBuffInput skill model.Skills.[skill].Buff dispatch
                      viewProfessions model.SkillLevelPolicy model.Skills.[skill] dispatch
                      viewSkillDistribution model skill ] ]
            checkboxText "Ignore Profession Relationships" ToggleIgnoreProfessionRelationships model.IgnoreProfessionRelationships dispatch ]

    | Crops ->
        div
          [ classBaseList
              "sidebar-table"
              [ "hidden", model.SelectedCrop.IsSome
                "open", model.SidebarOpen ] ]
          [ div [ ClassName "table-filters" ]
              [ checkboxText "Show Invalid Crops" ToggleShowInvalidCrops model.ShowInvalidCrops dispatch
                checkboxDisabledText "Show Out of Season Crops" (not model.ShowInvalidCrops) ToggleShowOutOfSeasonCrops model.ShowOutOfSeasonCrops dispatch ]
            let cropCell = tableCell (Crop.nameOf >> Some >> SetSelectedCrop) dispatch
            let cropHeaderCell = tableHeaderCell SetCropSort dispatch
            viewTable
              "table-crop"
              [ 0.05, th [] [ str "" ], tableCheckbox ToggleCropSelected Crop.nameOf Crop.selected dispatch >> td []
                0.4, cropHeaderCell [ str "Crop" ] CropSort.ByName, cropCell (tableImage Crop.image Crop.name)
                0.1, cropHeaderCell [ str "Growth Time" ] TotalGrowthTime, cropCell (Crop.totalGrowthTime >> ofInt >> List.singleton)
                0.1, cropHeaderCell [ str "Regrow Time" ] RegrowTime, cropCell (Crop.regrowTime >> Option.bind (ofInt >> Some) >> ofOption >> List.singleton)
                0.15, cropHeaderCell [ str "Seasons" ] Seasons,
                  cropCell
                    (fun crop ->
                      [ for season in Season.all do
                          if Crop.seasons crop |> Set.contains season then
                            img
                              [ classBase "season" "disabled" (Crop.selectedSeasons crop |> Set.contains season |> not)
                                Src <| "img/Seasons/" + Season.name season + ".png" ]
                          else
                            span [ ClassName "season-slot" ] [] ] )
                0.20, cropHeaderCell [ str "Seed Price" ] SeedPrice, cropCell (Crop.priceFrom >> viewPriceIcons model) ]
              Crop.name
              (Model.cropValid model >> not)
              (Model.sortedCrops model) ]

        match model.SelectedCrop with
        | Some crop ->
            let crop = model.Crops.[crop]
            div [ classBase "sidebar-info" "open" model.SidebarOpen ]
              [ div []
                  [ yield! tableImage Crop.imageOfName ofName (Option.get model.SelectedCrop)
                    ofOption (Option.bind (str >> Some) (Crop.flags crop)) ]
                
                viewTabs "crop-tabs" SetCropTab CropTab.all model.CropTab dispatch

                let baseCrop = Crop.baseCrop crop

                match model.CropTab with
                | Growth ->
                    ul []
                      [ str "Seasons:"
                        for season in baseCrop.Seasons do
                          li []
                            [ img
                                [ classBase "season" "disabled" (not <| baseCrop.SelectedSeasons.Contains season)
                                  Src <| "img/Seasons/" + Season.name season + ".png" 
                                  OnClick <| ignore ] ] ]
                    div []
                      [ str <| sprintf "Growth Time: %i (%s)" baseCrop.TotalGrowthTime (baseCrop.GrowthStages |> listToString) ]
                    match Crop.regrowTime crop with
                    | Some time ->
                        div []
                          [ str <| sprintf "Regrow Time: %i" time ]
                    | None -> nothing
                    if not baseCrop.GrowthMultipliers.IsEmpty then
                      div []
                        [ str "Growth Multipliers:"
                          ul []
                            [ for multipler in baseCrop.GrowthMultipliers do 
                                li []
                                  [ match multipler with
                                    | Profession (skill, name) ->
                                        viewGrowthMultiplier ("img/Skills/" + ofName name + ".png") (Model.professionActive model skill name) name
                                    | Raw name ->
                                        viewGrowthMultiplier ("img/Multipliers/" + ofName name + ".png") (model.RawMultipliers.[name].Selected) name ] ] ]
                    match crop with
                    | Bush b ->
                        div []
                          [ str <| sprintf "Harvest Days: %i - %i" b.HarvestDayStart b.HarvestDayEnd ]
                    | _ -> nothing
                | SeedPrices ->
                    div []
                      [ div []
                          [ str "Seed Prices:"
                            viewPrices model baseCrop.PriceFrom ]
                        div []
                          [ str "Best Seed Price(s):"
                            yield! viewPriceIcons model baseCrop.PriceFrom ] ]
                | Products -> span [] []
                | Replants -> span [] []

                button [ OnClick <| fun _ -> dispatch <| SetSelectedCrop None ]
                  [ str "Back" ] ]
        | None -> nothing


    | Fertilizers ->
        div
          [ classBaseList
              "sidebar-table"
              [ "hidden", model.SelectedFertilizer.IsSome
                "open", model.SidebarOpen ] ]
          [ let fertilizerCell = tableCell (Fertilizer.nameOf >> Some >> SetSelectedFertilizer) dispatch
            let fertilizerHeaderCell = tableHeaderCell SetFertilizerSort dispatch
            viewTable
              "table-fertilizer"
              [ 0.05, th [] [ str "" ], tableCheckbox ToggleFertilizerSelected Fertilizer.nameOf Fertilizer.selected dispatch >> td []
                0.5, fertilizerHeaderCell [ str "Fertilizer" ] FertilizerSort.ByName, fertilizerCell (tableImage Fertilizer.image Fertilizer.name)
                0.1, fertilizerHeaderCell [ str "Quality" ] Quality, fertilizerCell (Fertilizer.quality >> ofInt >> List.singleton)
                0.1, fertilizerHeaderCell [ str "Speed Bonus" ] Speed, fertilizerCell (Fertilizer.speed >> percent >> str >> List.singleton)
                0.25, fertilizerHeaderCell [ str "Price" ] Price, fertilizerCell (Fertilizer.priceFrom >> viewPriceIcons model) ]
              Fertilizer.name
              (Model.fertilizerValid model >> not)
              (Model.sortedFertilizers model)

            div [ ClassName "fertilizer-settings" ]
              [ checkboxText "Account for Fertilizer Cost" ToggleAccountForFertilizerCost model.AccountForFertilizerCost dispatch
                selectFertilizerOption
                  model.FertilizerList
                  (Some "Starting Fertilizer")
                  SetStartingFertilizer
                  model.StartingFertilizer
                  dispatch ] ]

        match model.SelectedFertilizer with
        | Some fert ->
            let fertilizer = model.Fertilizers.[fert]
            div [ classBase "sidebar-info" "open" model.SidebarOpen ]
              [ div []
                  (tableImage Fertilizer.image Fertilizer.name fertilizer)
                div []
                  [ span [] [ str <| sprintf "Quality: %i" fertilizer.Quality ]
                    span [] [ str <| sprintf "Speed Bonus: %s" (percent fertilizer.Speed) ] ]
                div []
                  [ str "Crop Quality Distribution:"
                    Skill.farmingDistributionWith fertilizer model.Skills.[Farming]
                    |> viewDistribution ]
                if not fertilizer.PriceFrom.IsEmpty then
                  div []
                    [ div []
                        [ str "Prices:"
                          viewPrices model fertilizer.PriceFrom ]
                      div []
                        [ str "Best Price(s):"
                          yield! viewPriceIcons model fertilizer.PriceFrom ] ]
                button [ OnClick <| fun _ -> dispatch <| SetSelectedFertilizer None ]
                  [ str "Back" ] ]
        | None -> nothing

    | Buy ->
        div [ classBase "sidebar-content" "open" model.SidebarOpen ]
          [ buySources model.BuySourceList model.BuySources dispatch
            ul [ ClassName "match-condition-list" ]
              [ for relative in model.PriceMultiplierList do
                checkboxText (ofName relative) (ToggleRelativePrice relative) model.PriceMultipliers.[relative].Selected dispatch ] ]

    | Sell ->
        div [ classBase "sidebar-content" "open" model.SidebarOpen ]
          [ ul [ ClassName "source-list" ]
              [ li []
                  [ checkboxWith (buyAndSellIcon "Raw Crop") ToggleSellRawCrop model.SellRawCrop dispatch ]
                for name in model.ProcessorList do
                  processor model name dispatch
                li []
                  [ checkboxWith (buyAndSellIcon "Seed Maker") ToggleSellSeedsFromSeedMaker model.SellSeedsFromSeedMaker dispatch
                    ] ] ]
                    //viewAlerts "SellSeedsFromSeedMaker" (Model.seedMakerAlert model model.SellSeedsFromSeedMaker) ] ] ]

    | Replant ->
        div [ classBase "sidebar-content" "open" model.SidebarOpen ]
          [ replants model dispatch
            checkboxText "Account For Replant" ToggleAccountForReplant model.AccountForReplant dispatch ]

    | Date ->
        div [ classBase "sidebar-content" "open" model.SidebarOpen ]
          [ date "Start Date" SetStartSeason SetStartDay model.StartDate dispatch
            date "End Date" SetEndSeason SetEndDay model.EndDate dispatch ]

    | Settings ->
        div [ classBase "sidebar-content" "open" model.SidebarOpen ]
          [ selectString ProfitMode.all (Some "Compare Stonks Using") SetProfitMode model.ProfitMode dispatch
            checkboxText "Greenhouse Mode" ToggleGreenhouse model.Greenhouse dispatch
            selectString RequirementPolicy.all (Some "Skill Level Unlocks") SetSkillLevelPolicy model.SkillLevelPolicy dispatch
            checkboxText "Special Charm" ToggleSpecialCharm model.SpecialCharm dispatch
            label [ ClassName "setting-input" ]
              [ str "Luck Buff:"
                input
                  [ Type "number"
                    Min 0
                    valueOrDefault model.LuckBuff
                    ClassName "setting-number-input"
                    OnChange <| fun b -> dispatch <| SetLuckBuff !!b.Value ] ]
            label [ ClassName "setting-input" ]
              [ str "Giant Crop Checks Per Tile:"
                input
                  [ Type "number"
                    Min 0.0
                    Max 9.0
                    Step "any"
                    valueOrDefault model.GiantCropChecksPerTile
                    ClassName "setting-number-input"
                    OnChange <| fun c -> dispatch <| SetGiantCropChecksPerTile !!c.Value ] ] ]

    | Mod ->
        div [ classBase "sidebar-content" "open" model.SidebarOpen ]
          [ checkboxText "Quality Products" ToggleQualityProducts model.QualityProducts dispatch
            ul []
              [ for processor in model.ProcessorList do
                  li []
                    [ checkboxCss
                        (classBase "checkbox-label" "disabled" (not model.QualityProducts))
                        (buyAndSellIcon <| ofName processor)
                        (TogglePreservesQuality processor)
                        model.Processors.[processor].PreservesQuality
                        dispatch ] ]
            checkboxText "Quality Seed Maker" ToggleQualitySeedMaker model.QualitySeedMaker dispatch
            str "(Average) Seed Amounts:"
            ul []
              [ for KeyValue(quality, amount) in model.QualitySeedMakerAmounts do
                  li []
                    [ label [ classBase "" "disabled" (not model.QualitySeedMaker) ]
                        [ str <| string quality + ":"
                          input
                            [ Type "number"
                              Min 0
                              valueOrDefault amount
                              OnChange (fun v -> dispatch <| SetQualitySeedMakerAmount (quality, !!v.Value)) ] ] ] ] ] ]
  //lazyView2With (fun oldModel newModel -> (not oldModel.SidebarOpen && not newModel.SidebarOpen) || oldModel = newModel) sidebarContent

let cover sidebarOpen dispatch =
  div
    [ classBase "cover" "open" sidebarOpen
      OnClick <| fun _ -> dispatch CloseSidebar ]
    []

let sidebar model dispatch =
  div [ classModifier "sidebar" "open" model.SidebarOpen ]
    [ viewTabsWith string "sidebar-tabs" SetSidebarTab SidebarTab.all (fun t -> model.SidebarOpen && t = model.SidebarTab) dispatch
      yield! sidebarContent model dispatch ]

let view model dispatch =
  match model.Page with
  | Home ->
      div []
        [ str "Select a mode:"
          for mode in Mode.all do
            a [ Href (mode |> Mode |> Page.url) ]
              [ button [ ClassName "mode" ]
                  [ str <| string mode ] ] ]
  | Mode mode ->
      div []
        [ span [] [ str <| string mode ]
          div []
            [ match mode with
              | Compare ->
                  div []
                    [ selectString CompareMode.all (Some "Compare") SetCompareMode model.CompareMode dispatch
                      checkboxText "Show Unprofitable Combinations" ToggleShowUnprofitableCombos model.ShowUnprofitableCombos dispatch 
                      if model.CompareMode = CompareCrops then
                        selectFertilizerOption (Model.activeFertilizers model) (Some "Select a Fertilizer") SetCompareCropsUsingFertilizer model.CompareCropsUsingFertilizer dispatch
                      elif model.CompareMode = CompareFertilizers then
                        selectString (Model.activeCrops model) (Some "Select a Crop") SetCompareFertilizersUsingCrop model.CompareFertilizersUsingCrop dispatch ]
                  ul [ Style [ MarginLeft "250px" ] ]
                    [ ofList
                        [ let combos =
                            match model.CompareMode with
                            | Combos -> Model.doComboCompare model
                            | CompareCrops ->
                                match model.CompareCropsUsingFertilizer with
                                | Some fert ->
                                    let fertilizer = model.Fertilizers.[fert]
                                    if Model.fertilizerActive model fertilizer
                                    then Model.doCropCompare model (Some fertilizer)
                                    else []
                                | None -> Model.doCropCompare model None
                            | CompareFertilizers ->
                                let crop = model.Crops.[model.CompareFertilizersUsingCrop]
                                if Model.cropActive model crop
                                then Model.doFertilizerCompare model crop
                                else []

                          for crop, fert, profit in combos do
                            li [ Key (ofName crop + " " + Fertilizer.nameOfOption fert) ]
                              [ str <| ofName crop
                                br []
                                str <| Fertilizer.nameOfOption fert
                                br []
                                ofFloat profit ] ] ]
              | _ -> br [] ]
          a [ Href <| Page.url Help ]
            [ str "Help" ]
          button [ OnClick <| fun _ -> dispatch Calculate ]
            [ str "Calculate" ]
          cover model.SidebarOpen dispatch
          sidebar model dispatch ]
  | Help ->
      span [] [ str "Help! I need somebody!" ]

//--App--
open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Elmish.UrlParser

Program.mkProgram AppData.initialModel update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.toNavigable (parseHash Page.parseUrl) urlUpdate
|> Program.withReactBatched "elmish-app"
|> Program.run