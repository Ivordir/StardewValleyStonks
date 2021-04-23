module StardewValleyStonks.View

open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
// open Browser
open Elmish.React
// open Elmish.React.Helpers


let gold = sprintf "%ig"
let percent value = sprintf "%f%%" (value * 100.0)
let percent2 value = sprintf "%.2f%%" (value * 100.0)
let floatTrunc = sprintf "%.5f"

let inline ofOptionMap mapping = Option.map mapping >> ofOption

let inline lazyElm element = lazyView (fun () -> element) ()

let classBase baseClass single add = classBaseList baseClass [ single, add ]

let checkboxCss css alsoDisplay (message: Message) isChecked dispatch =
  label [ css ]
    [ input
        [ Type "checkbox"
          Checked isChecked
          OnChange <| fun _ -> dispatch message ]
      img
        [ ClassName "checkbox-img"
          Src <| if isChecked then "img/UI/Checkedbox.png" else "img/UI/Checkbox.png" ]
      yield! alsoDisplay ]

let checkboxWith = checkboxCss (ClassName "checkbox-label")
let checkboxDisabledWith alsoDisplay disabled =
  checkboxCss (classBase "checkbox-label" "disabled" disabled) alsoDisplay
let checkboxDisabled = checkboxDisabledWith []
let checkboxDisabledText text = checkboxDisabledWith [ str text ]
let checkbox = checkboxWith []
let checkboxText text = checkboxWith [ str text ]


module Image =
  let path = sprintf "img/%s/%s.png"
  let skillRoot = path "Skills"
  let professionPath: Profession -> _ = (!!) >> skillRoot
  let skillPath = Skill.name >> skillRoot
  let fertilizerRoot = path "Fertilizers"
  let fertilizerPath = Fertilizer.name >> fertilizerRoot
  let cropRoot = path "Crops"
  let cropPath = Crop.name >> cropRoot
  let sourceRoot = path "Sources"
  let sourcePath: Source -> _ = (!!) >> sourceRoot
  let processorRoot = path "Processors"
  let processorPath = Processor.name >> sourceRoot

  let at path = img [ Src <| path ]

  let private andName pathFun name =
    [ at <| pathFun name
      str name ]
  let profession: Profession -> _ = (!!) >> andName skillRoot 
  let skill = Skill.name >> andName skillRoot
  let fertilizer = Fertilizer.name >> andName fertilizerRoot
  let fertilizerName: Fertilizer name -> _ = (!!) >> andName fertilizerRoot
  let crop = Crop.name >> andName cropRoot
  let cropName: Crop name -> _ = (!!) >> andName cropRoot
  let source: Source -> _ = (!!) >> andName sourceRoot
  let processor = Processor.name >> andName processorRoot
  let processorName: Processor name -> _ = (!!) >> andName processorRoot
  let processorOption = Processor.nameOption >> andName processorRoot
  let processorNameOption: Processor name option -> _ = defaultMap "Raw Crop" (!!) >> andName processorRoot


let viewTab toString msg tab currentTab dispatch =
  button
    [ if currentTab = tab then ClassName "active"
      OnClick <| fun _ -> dispatch <| msg tab ]
    [ str <| toString tab ]

let viewTabsWith toString css msg tabs currentTab dispatch =
  div [ ClassName css ] (tabs |> List.map (fun tab ->
    lazyView2 (viewTab toString msg tab) currentTab dispatch))

let inline viewTabs css = viewTabsWith string css

let warningIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Warning.png" ]

let errorIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Error.png" ]

let tryParse parse message dispatch (event: Browser.Types.Event) =
  try parse event.Value |> message |> dispatch
  with | _ -> ()

let tryParseInt = tryParse int
let tryParseFloat = tryParse float

let selectBase toValue (toString: 't -> _) parse list (value: 't) dispatch =
  select
    [ valueOrDefault <| toValue value
      OnChange <| parse dispatch ]
    (list |> Seq.map (fun item ->
      option [ Value (toValue item) ]
        [ str <| toString item ] |> lazyElm ))

//let selectWith toString = selectBase toString toString

// For types that do not erase into strings
//let inline selectParse parse = selectWith string parse

// For types that erase into strings (i.e. [<StringEnum>] and [<Erase>] on DU's)
let inline selectString message = selectBase string string (fun d ev -> d <| message !!ev.Value)

//let inline selectStringOptionWith none list = selectWith (defaultMap none string) (parseOption none (!!)) (listWithNone list)

//let inline selectStringOption list = selectStringOptionWith "None" list

// let selectFertilizerOption list =
//   selectWith
//     (string |> defaultMap Fertilizer.none |> defaultMap "Any")
//     ((!!) |> parseOption Fertilizer.none |> parseOption "Any")
//     (list |> listWithNone |> listWithNone)

let skillLevelInput name level dispatch =
  let onChange = OnChange <| tryParseInt (tuple2 name >> SetSkillLevel) dispatch
  label []
    [ str "Level:"
      input
        [ Type "number"
          Min 0
          Max 10
          valueOrDefault level
          onChange ]
      input
        [ Type "range"
          Min 0
          Max 10
          valueOrDefault level
          onChange ] ]

let skillBuffInput name buff dispatch =
  label []
    [ str "Buff:"
      input
        [ Type "number"
          Min 0
          valueOrDefault buff
          OnChange <| tryParseInt (tuple2 name >> SetSkillBuff) dispatch ] ]

let viewProfession skillLevelPolicy skill profession selected unlocked dispatch =
  button
    [ classList
        [ "active", selected
          "disabled", skillLevelPolicy = Enforce && not unlocked ]
      OnClick <| fun _ -> dispatch <| ToggleProfession (skill, profession) ]
    [ if selected && not unlocked && skillLevelPolicy = Warn then
        warningIcon
      yield! Image.profession profession ]

let viewDistribution distribution =
  div [ ClassName "quality-distribution" ]
    [ let dist = distribution |> Seq.mapi (enum<Quality> >> tuple2) |> Seq.filter (snd >> (=) 0.0 >> not)
      div [ ClassName "bars" ] (dist |> Seq.map (fun (quality, prob) ->
        div
          [ Style
              [ Width (percent prob)
                BackgroundColor (Quality.color quality) ] ]
          [] ))
      div [ ClassName "probs" ] (dist |> Seq.map (fun (quality, prob) ->
        div
          [ Style
              [ Color (Quality.color quality)
                FlexGrow prob ] ]
          [ prob |> percent2 |> str ] )) ]

let viewSkill professionActive skillLevelPolicy skill dispatch =
  let prof p =
    lazyView3
      (viewProfession skillLevelPolicy skill.Which p)
      (skill.SelectedProfessions.Contains p)
      (Skill.professionUnlocked skill p)
      dispatch
  div []
    [ span []
        (Image.skill skill) |> lazyElm
      skillLevelInput skill.Which skill.Level dispatch
      skillBuffInput skill.Which skill.Buff dispatch
      div [ ClassName "professions" ]
        [ div []
            [ prof skill.Lvl5Profession ]
          div []
            [ prof skill.Lvl10ProfessionA
              skill.Lvl10ProfessionB |> ofOptionMap prof ] ]
      lazyView viewDistribution (Skill.qualityDistribution professionActive skill) ]

let colWidths widths =
  colgroup [] (widths |> List.map (fun width ->
    col [ Style [ Width (percent width) ] ] ))

let viewPriceData multiplierSelected prices =
  let value = Price.value multiplierSelected
  [ match Prices.lowest value prices with
    | Some lowest ->
        ofInt lowest
        ul [] (prices.Selected |> Seq.filter (value >> (=) lowest) |> Seq.map (fun price ->
          li []
            [ Image.at <| Image.sourceRoot !!price.Source ] ))
    | None -> str "No Prices" ]

let selectionTable widths selectAllMsg selectMsg headers itemDisplay row (selection: _ Set) (items: (_ name * _) seq) dispatch =
  table [ ClassName "select" ]
    [ colWidths widths
      thead []
        [ tr []
            [ th []
                [ checkbox selectAllMsg (selection.Count = Seq.length items) dispatch ]
              yield! headers ] ]
      tbody []
        [ items |> Seq.map (fun (name, item) ->
            tr [ Key !!name ]
              [ td []
                  [ checkbox (selectMsg name) (selection.Contains name) dispatch ]
                td []
                  (itemDisplay !!name)
                yield! row item ] )
          |> List.ofSeq
          |> ofList ] ]

let selectFertilizerTable farmingDist multiplierSelected selection order dispatch =
  selectionTable
    [ 0.1; 0.3; 0.1; 0.1; 0.4 ]
    ToggleAllFertilizers
    ToggleFertilizer
    [ th [ OnClick <| fun _ -> dispatch <| SetFertilizerSort FertilizerName ]
        [ str "Fertilizer" ]
      th [ OnClick <| fun _ -> dispatch <| SetFertilizerSort Cost ]
        [ str "Lowest Price" ]
      th [ OnClick <| fun _ -> dispatch <| SetFertilizerSort Speed ]
        [ str "Speed" ]
      th [ OnClick <| fun _ -> dispatch <| SetFertilizerSort Quality ]
        [ str "Qualities" ] ]
    Image.fertilizerName
    (fun fert ->
      [ td []
          (Fertilizer.prices fert |> viewPriceData multiplierSelected)
        td []
          [ ofFloat fert.Speed ]
        td []
          [ farmingDist fert.Quality |> viewDistribution ] ] )
    selection
    order
    dispatch


let selectCropTable multiplierActive multiplierSelected selection order dispatch =
  selectionTable
    [ 0.1; 0.25; 0.15; 0.1; 0.1; 0.3 ]
    ToggleAllCrops
    ToggleCrop
    [ th [ OnClick <| fun _ -> dispatch <| SetCropSort CropName ]
        [ str "Crop" ]
      th [ OnClick <| fun _ -> dispatch <| SetCropSort SeedCost ]
        [ str "Lowest Seed Price" ]
      th [ OnClick <| fun _ -> dispatch <| SetCropSort TotalGrowthTime ]
        [ str "Growth Time" ]
      th [ OnClick <| fun _ -> dispatch <| SetCropSort RegrowTime ]
        [ str "Regrow Time" ]
      th [ OnClick <| fun _ -> dispatch <| SetCropSort Seasons ]
        [ str "Seasons" ] ]
    Image.cropName
    (fun crop ->
      [ td []
          (Crop.seedPrices crop |> defaultMap [ str "No Prices" ] (viewPriceData multiplierSelected))
        td []
          [ let actualTime = Crop.growthTime multiplierActive crop
            let total = Crop.totalGrowthTime crop
            ofInt actualTime
            if actualTime <> total then
              br []
              span [ Style [ ] ]
                [ ofInt total ] ]
        td []
          [ Crop.regrowTime crop |> ofOptionMap ofInt ]
        td []
          [ let growsIn = Crop.seasons crop ||> Season.isBetween
            yield! Season.all |> Seq.map (fun season ->
              if growsIn season then
                img [ Src <| "img/Seasons/" + Season.name season + ".png" ]
              else
                span [ ClassName "season-slot" ] [] ) ] ] )
    selection
    order
    dispatch

let priceTable typeName priceMsg toggleAllMsg image prices multiplierSelected sources (items: ('a name * 'a) seq) dispatch =
  table [ ClassName "prices" ]
    [ colWidths [ 0.2; 0.2 ]
      thead []
        [ tr []
            [ th []
                [ str typeName ]
              th []
                [ str "Lowest Price" ]
              let allSelected source =
                items |> Seq.forall (snd >> prices >> Option.forall (Prices.sourceSelected source))
              yield! sources |> Seq.map (fun source ->
                th []
                  [ checkbox (toggleAllMsg source) (allSelected source) dispatch
                    yield! Image.source source ] ) ] ]
      tbody [] (items |> Seq.map (fun (name, item) ->
        tr []
          [ td []
              (image name)
            match prices item with
            | Some prices ->
                td []
                  (viewPriceData multiplierSelected prices)
                yield! sources |> Seq.map (fun source ->
                  td []
                    [ prices.From.TryFind source
                      |> ofOptionMap (fun price ->
                        checkboxWith
                          [ let p, alt = Price.valueWithAlternate multiplierSelected price
                            str <| gold p
                            match alt with
                            | Some a ->
                                br []
                                span [ Style [] ]
                                  [ str <| gold a ]
                            | None -> nothing ]
                          (priceMsg (name, price))
                          (prices.Selected.Contains price)
                          dispatch) ] )
            | None -> yield! [] ] )) ]

let dayInput message day dispatch =
  input
    [ Type "number"
      Min 1
      Max 28
      valueOrDefault day
      OnChange <| tryParseInt message dispatch ]

let date text seasonMsg dayMsg (Date(season, day)) dispatch =
  div [ ClassName "date" ]
    [ str <| text + ":"
      lazyView2 (selectBase int Season.name (tryParseInt (Season.ofInt >> seasonMsg)) Season.all) season dispatch
      dayInput dayMsg day dispatch ]

let luckBuffInput buff dispatch =
  label []
    [ str "Luck Buff:"
      input
        [ Type "number"
          Min 0
          valueOrDefault buff
          OnChange <| tryParseInt SetLuckBuff dispatch ] ]

let giantCropInput checks dispatch =
  label []
    [ str "Giant Crop Checks Per Tile:"
      input
        [ Type "number"
          Min 0.0
          Max 9.0
          Step "any"
          valueOrDefault checks
          OnChange <| tryParseFloat SetGiantCropChecksPerTile dispatch ] ]

let viewQualityProducts qualityProcessors qualityProducts processors dispatch =
  div [ ClassName " quality-products" ]
    [ lazyView2 (checkboxText "Quality Products" ToggleQualityProducts) qualityProducts dispatch
      ul [ if not qualityProducts then ClassName "disabled" ] (qualityProcessors |> Seq.map (fun processor -> 
        li []
          [ lazyView2
              (checkboxWith (Image.processorName processor) (ToggleQualityProcessor processor))
              (processors |> Map.find processor |> Processor.qualityProductsSelected)
              dispatch ] )) ]

let seedMakerAmountInput quality amount dispatch =
  li []
    [ label []
        [ str <| string quality + ":"
          input
            [ Type "number"
              Min 0
              valueOrDefault amount
              OnChange <| tryParseInt (tuple2 quality >> SetQualitySeedMakerAmount) dispatch ] ] ]

let viewQualitySeedMaker qualitySeedMaker (seedMakerAmounts: _ array) dispatch =
  div [ ClassName "quality-seedMaker" ]
    [ lazyView2 (checkboxText "Quality Seed Maker" ToggleQualitySeedMaker) qualitySeedMaker dispatch
      str "(Average) Seed Amounts:"
      ul [ if not qualitySeedMaker then ClassName "disabled" ]
        [ for i = 0 to seedMakerAmounts.Length - 1 do
            seedMakerAmountInput (enum i) seedMakerAmounts.[i] dispatch ] ]

let settings ((ui, data) as model) dispatch =
  div [ ClassName "settings" ]
    [ lazyView2 (viewTabs "tabs" SetSettingsTab SettingsTab.all) ui.SettingsTab dispatch
      match ui.SettingsTab with
      | Skills ->
          div [ ClassName "skills" ] (Map.values data.Skills |> Seq.map (fun skill ->
            viewSkill (DataModel.professionActive data) data.SkillLevelPolicy skill dispatch))
      | Crops ->
          lazyView2 (viewTabs "tabs" SetCropTableTab CropTableTab.all) ui.CropTableTab dispatch
          div []
            [ match ui.CropTableTab with
              | SelectCrops ->
                  selectCropTable
                    (DataModel.multiplierActive data)
                    data.SelectedPriceMultipliers.Contains
                    data.CropSelection
                    (Model.sortedCrops model)
                    dispatch
              | SeedPrices ->
                  priceTable
                    "Crop"
                    ToggleSeedPrice
                    ToggleSeedSource
                    Image.cropName
                    Crop.seedPrices
                    data.SelectedPriceMultipliers.Contains
                    data.SeedSources
                    (DataModel.cropOrder data)
                    dispatch
              | Products ->
                  table [ ClassName "products" ]
                    [ //colWidths [ 0.2; 0.2 ]
                      thead []
                        [ tr []
                            [ th [ ColSpan 2 ]
                                [ str "Crop" ]
                              let allSelected processor =
                                data.Crops |> Map.values |> Seq.collect Crop.rawCrops |> Seq.forall (RawCrop.processorSelected processor)
                              yield! data.ProductProcessors |> Seq.map (fun processor ->
                                th []
                                  [ checkbox (ToggleProductProcessor processor) (allSelected processor) dispatch
                                    yield! Image.processorNameOption processor ] ) ] ]

                      let row name rawCrop =
                        data.ProductProcessors |> Seq.map (fun processor ->
                          td []
                            [ rawCrop.Products.TryFind processor
                              |> ofOptionMap (fun product -> 
                                checkbox (ToggleProduct (name, rawCrop.Item, product)) (rawCrop.Selected.Contains product) dispatch) ] )
                      
                      tbody [] (data.Crops |> Map.keys |> Seq.collect (fun name ->
                        let rawCrops = data.Crops.[name] |> Crop.rawCrops
                        let count = Seq.length rawCrops
                        
                        [ tr []
                            [ if count > 1 then
                                td [ RowSpan count ]
                                  (Image.cropName name)
                                td []
                                  [ str (Seq.head rawCrops).Item.Name ]
                              else
                                td [ ColSpan 2 ]
                                  (Image.cropName name)
                              yield! Seq.head rawCrops |> row name ]
                          yield! Seq.tail rawCrops |> Seq.map (fun rawCrop ->
                            tr []
                              [ td []
                                  [ str rawCrop.Item.Name ]
                                yield! row name rawCrop ] ) ] )) ]
              | Replants ->
                  table [ ClassName "products" ]
                    [ //colWidths [ 0.2; 0.2 ]
                      thead []
                        [ tr []
                            [ th [ ColSpan 2 ]
                                [ str "Crop" ]
                              th []
                                [ checkbox ToggleAllBuySeeds (data.Crops |> Map.values |> Seq.forall (Crop.seedSelected >> Option.forall id)) dispatch
                                  str "Buy Seeds" ]
                              let allChecked processor =
                                data.Crops |> Map.values |> Seq.collect Crop.rawCrops |> Seq.forall (RawCrop.replantProcessorSelected processor)
                              yield! data.ProductReplants |> Seq.map (fun processor ->
                                th []
                                 [ checkbox (ToggleReplantProcessor processor) (allChecked processor) dispatch
                                   yield! Image.processorNameOption processor ] ) ] ]

                      let row name rawCrop =
                        data.ProductReplants |> Seq.map (fun processor ->
                          td []
                            [ match rawCrop.Replant with
                              | Some (product, s) when product.Processor = processor ->
                                  checkbox (ToggleReplant (name, rawCrop.Item)) s dispatch
                              | _ -> yield! [] ] )
                      
                      tbody [] (data.Crops |> Map.keys |> Seq.collect (fun name ->
                        let crop = data.Crops.[name]
                        let rawCrops = Crop.rawCrops crop
                        let count = Seq.length rawCrops
                        
                        [ tr []
                            [ if count > 1 then
                                td [ RowSpan count ]
                                  (Image.cropName name)
                                td []
                                  [ str (Seq.head rawCrops).Item.Name ]
                              else
                                td [ ColSpan 2 ]
                                  (Image.cropName name)
                              td [ RowSpan count ]
                                [ Crop.seedSelected crop |> ofOptionMap (fun s -> checkbox (ToggleBuySeeds name) s dispatch) ]
                              yield! Seq.head rawCrops |> row name ]
                          yield! Seq.tail rawCrops |> Seq.map (fun rawCrop ->
                            tr []
                              [ td []
                                  [ str rawCrop.Item.Name ]
                                yield! row name rawCrop ] ) ] )) ] ]
      | Fertilizers ->
          div [ ClassName "fertilizers" ]
            [ lazyView2 (viewTabs "tabs" SetFertilizerTableTab FertilizerTableTab.all) ui.FertilizerTableTab dispatch
              match ui.FertilizerTableTab with
              | SelectFertilizers ->
                  selectFertilizerTable
                    (Skill.farmingDistribution data.Skills.[Farming])
                    data.SelectedPriceMultipliers.Contains
                    data.FertilizerSelection
                    (Model.sortedFertilizers model)
                    dispatch
              | FertilizerPrices ->
                  priceTable
                    "Fertilizer"
                    ToggleFertilizerPrice
                    ToggleFertilizerSource
                    Image.fertilizerName
                    (Fertilizer.prices >> Some)
                    data.SelectedPriceMultipliers.Contains
                    data.FertilizerSources
                    (DataModel.fertilizerOrder data)
                    dispatch ]
      | DateTab ->
          div [ ClassName "date" ]
            [ date "Start Date" SetStartSeason SetStartDay data.StartDate dispatch
              date "End Date" SetEndSeason SetEndDay data.EndDate dispatch ]
      | Misc ->
          div [ ClassName "misc" ]
            [ //checkboxText "Greenhouse Mode" ToggleGreenhouse model.Greenhouse dispatch
              lazyView2 (checkboxText "Special Charm" ToggleSpecialCharm) data.SpecialCharm dispatch
              luckBuffInput data.LuckBuff dispatch
              giantCropInput data.GiantChecksPerTile dispatch
              div []
                [ str "Skill Level Unlocks"
                  selectString SetSkillLevelPolicy RequirementPolicy.all data.SkillLevelPolicy dispatch ]
              lazyView2 (checkboxText "Ignore Profession Relationships" ToggleIgnoreProfessionRelationships) ui.IgnoreProfessionRelationships dispatch
              div []
                []
              div [] (data.PriceMultipliers |> Seq.map (fun multiplier ->
                checkboxText (PriceMultiplier.name multiplier) (TogglePriceMultiplier multiplier) (data.SelectedPriceMultipliers.Contains multiplier) dispatch))
              div [] (data.RawMultipliers |> Seq.map (fun multipler ->
                checkboxText (RawMultiplier.name multipler) (ToggleRawMultiplier multipler) (data.SelectedRawMultipliers.Contains multipler) dispatch)) ]
      | Mod ->
          div [ ClassName "mod" ]
            [ lazyView3 (viewQualityProducts data.QualityProcessors) data.QualityProducts data.Processors dispatch
              viewQualitySeedMaker data.QualitySeedMaker data.QualitySeedMakerAmounts dispatch ] ]
    // | Crops ->
    //     div
    //       [ classBaseList
    //           "sidebar-table"
    //           [ "hidden", model.SelectedCrop.IsSome
    //             "open", model.SidebarOpen ] ]
    //       [ div [ ClassName "table-filters" ]
    //           [ checkboxText "Show Invalid Crops" ToggleShowInvalidCrops model.ShowInvalidCrops dispatch
    //             checkboxDisabledText "Show Out of Season Crops" (not model.ShowInvalidCrops) ToggleShowOutOfSeasonCrops model.ShowOutOfSeasonCrops dispatch ]

    //     match model.SelectedCrop with
    //     | Some name ->
    //         let crop = model.Crops.[name]
    //         div [ classBase "sidebar-info" "open" model.SidebarOpen ]
    //           [ div []
    //               [ yield! tableImage Crop.imageOfName ofName (Option.get model.SelectedCrop)
    //                 ofOption <| Option.bind (str >> Some) (Crop.description crop) ]
    //             viewTabs "crop-tabs" SetCropTab CropTab.all model.CropTab dispatch

    //             let baseCrop = Crop.baseCrop crop
    //             match model.CropTab with
    //             | Growth ->
    //                 ul [ ClassName "seasons" ]
    //                   [ str "Seasons:"
    //                     for season in baseCrop.Seasons do
    //                       li []
    //                         [ img
    //                             [ if not <| baseCrop.SelectedSeasons.Contains season then ClassName "disabled"
    //                               Src <| "img/Seasons/" + Season.name season + ".png"
    //                               OnClick <| fun _ -> dispatch <| (ToggleSeason season |> tuple2 name |> CropUpdate) ] ] ]
    //                 div []
    //                   [ str <| sprintf "Growth Time: %i" baseCrop.TotalGrowthTime ] //(%s) (baseCrop.GrowthStages |> listToString) ]
    //                 match Crop.regrowTime crop with
    //                 | Some time ->
    //                     div []
    //                       [ str <| sprintf "Regrow Time: %i" time ]
    //                 | None -> nothing
    //                 if not baseCrop.GrowthMultipliers.IsEmpty then
    //                   div []
    //                     [ str "Growth Multipliers:"
    //                       ul []
    //                         [ for multipler in baseCrop.GrowthMultipliers do 
    //                             li []
    //                               [ match multipler with
    //                                 | Profession (skill, name) ->
    //                                     viewGrowthMultiplier ("img/Skills/" + ofName name + ".png") (Model.professionActive model skill name) name
    //                                 | Raw name ->
    //                                     viewGrowthMultiplier ("img/Multipliers/" + ofName name + ".png") (model.RawMultipliers.[name].Selected) name ] ] ]
    //                 match crop with
    //                 | RegrowCrop r ->
    //                     if r.IndoorsOnly then
    //                       div []
    //                         [ str "Only Grows Indoors" ]
    //                 | Bush b ->
    //                     div []
    //                       [ str <| sprintf "Harvest Days: %i - %i" b.HarvestStartDay b.HarvestEndDay ]
    //                 | _ -> nothing
    //             | SeedPrices ->
    //                 div []
    //                   [ str "Seed Prices:"
    //                     togglePrices (tuple2 (Crop.nameOf crop) >> ToggleCropPrice) model baseCrop.PriceFrom dispatch
    //                     str "Best Seed Price(s):"
    //                     yield! viewPriceIcons model baseCrop.PriceFrom ]
    //             | Products ->
    //                 match Crop.harvestCrops crop with
    //                 | [one] ->
    //                     productTable
    //                       model
    //                       (ToggleCropProduct >> tuple2 name >> CropUpdate)
    //                       (match Crop.seedMaker crop with
    //                        | Some seedMaker -> Some (seedMaker, SetSellSeedsFromSeedMaker >> tuple2 name >> CropUpdate)
    //                        | None -> None)
    //                       one
    //                       crop
    //                       dispatch
    //                 | multiple ->
    //                     ul []
    //                       [ for harvestCrop in multiple do
    //                           productTable
    //                             model
    //                             (ToggleCropProduct >> tuple2 name >> CropUpdate)
    //                             (match Crop.seedMaker crop with
    //                              | Some seedMaker -> Some (seedMaker, SetSellSeedsFromSeedMaker >> tuple2 name >> CropUpdate)
    //                              | None -> None)
    //                             harvestCrop
    //                             crop
    //                             dispatch ]
    //             | Replants ->
    //                 ul []
    //                   [ if not <| baseCrop.PriceFrom.IsEmpty then
    //                       li [ if not <| Model.buySeedsActive model baseCrop then ClassName "disabled" ]
    //                         [ selectOverride (buyAndSellIcon "Buy Seeds") (SetBuySeeds >> tuple2 name >> CropUpdate) baseCrop.BuySeeds dispatch ]
    //                     match Crop.seedMaker crop with
    //                     | Some seedMaker ->
    //                       li [ if not <| Model.seedMakerReplantActive model seedMaker then ClassName "disabled" ]
    //                         [ selectOverride (buyAndSellIcon "Seed Maker") (SetSeedMakerReplant >> tuple2 name >> CropUpdate) seedMaker.Replant dispatch ]
    //                     | None -> nothing
    //                     match Crop.rawPlant crop with
    //                     | Some raw ->
    //                         li [ if not <| Model.rawReplantActive model raw then ClassName "disabled" ]
    //                           [ selectOverride (buyAndSellIcon "Harvested Seed or Crop") (SetRawReplant >> tuple2 name >> CropUpdate) raw dispatch ]
    //                     | None -> nothing ]
    //             button [ OnClick <| fun _ -> dispatch <| SetSelectedCrop None ]
    //               [ str "Back" ] ]
    //     | None -> nothing

let modes =
  Mode.all |> List.map (fun mode ->
    a [ Href (mode |> Mode |> Page.url) ]
      [ str <| string mode ] )

module Re = Fable.Recharts

let pairImage pairs props =
  let key: string = props?payload?value
  match pairs key with
  | Some pair ->
      svg
        [ SVGAttr.X (props?x - 10)
          SVGAttr.Y props?y
          SVGAttr.Width 20
          SVGAttr.Height 40 ]
        [ image
            [ Href <| Image.cropRoot !!pair.Crop
              SVGAttr.Width 20
              SVGAttr.Height 20 ]
            []
          match Option.map (!!) pair.Fertilizer with
          | Some f ->
              image
                [ Href <| Image.fertilizerRoot f
                  SVGAttr.Width 20
                  SVGAttr.Height 20
                  SVGAttr.Y 20 ]
                []
          | None -> yield! [] ]
  | None -> nothing
 
let chartTooltip pairs props =
  if props?active then
    let key: string = props?label
    match pairs key with
    | Some pair ->
        div []
          [ str pair.Description ]
    | None -> nothing
  else
    nothing

let mutable graphBrushStart = 0
let mutable graphBrushEnd = 24
let indexChange i =
  graphBrushStart <- i?startIndex
  graphBrushEnd <- i?endIndex

// let debug model =
//   let crops = model.Crops |> Map.values |> Seq.toArray
//   let fertilizers = model.Fertilizers |> Map.values |> Seq.map Some |> Seq.append (Seq.singleton None) |> Seq.toArray

//   let rawCrops = crops |> Seq.collect Crop.rawCrops |> Seq.toArray

//   let startDate = model.StartDate
//   let endDate = model.EndDate

//   let farmingDist = Model.farmingDistribution model |> memoize
//   let forageDist = Model.foragingAmounts model

//   console.time "cached"
//   for _ = 1 to 10000 do
//     for crop in crops do
//       for fert in fertilizers do
//         let cached = profitPerHarvest fert crop
//         ignore cached
//   console.timeEnd "cached"

//   let pph =
//     let up = unitProfit
//     let temp =
//       Crop.profit
//         (RawCrop.profitPerHarvest up)
//         (RawCrop.profitPerHarvestNormal up)
//         model.CropAmounts
//         forageDist
//     fun fertilizer crop ->
//       temp (fertilizer |> Fertilizer.qualityOption |> farmingDist) crop
//   console.time "full"
//   for _ = 1 to 10000 do
//     for crop in crops do
//       for fert in fertilizers do
//         let full = pph fert crop
//         ignore full
//   console.timeEnd "full"
  
// let debugReplant model =
//   let crops = model.Crops |> Map.values |> Seq.toArray
//   let fertilizers = model.Fertilizers |> Map.values |> Seq.map Some |> Seq.append (Seq.singleton None) |> Seq.toArray

//   let rawCrops = crops |> Seq.collect Crop.rawCrops |> Seq.toArray

//   let startDate = model.StartDate
//   let endDate = model.EndDate

//   let farmingDist = Model.farmingDistribution model |> memoize
//   let forageDist = Model.foragingAmounts model

//   console.time "cached"
//   for _ = 1 to 10000 do
//     for crop in crops do
//       for fert in fertilizers do
//         let cached = profitPerHarvest fert crop
//         ignore cached
//   console.timeEnd "cached"

//   let pph =
//     let up = unitProfit
//     let temp =
//       Crop.profit
//         (RawCrop.profitPerHarvest up)
//         (RawCrop.profitPerHarvestNormal up)
//         model.CropAmounts
//         forageDist
//     fun fertilizer crop ->
//       temp (fertilizer |> Fertilizer.qualityOption |> farmingDist) crop
//   console.time "full"
//   for _ = 1 to 10000 do
//     for crop in crops do
//       for fert in fertilizers do
//         let full = pph fert crop
//         ignore full
//   console.timeEnd "full"

//   let uo = Model.unitOutput model
//   let pa = Model.processorActiveOption model

//   let rep =
//     let temp = RawCrop.replantCost (unitProfit, uo, pa)
//     replantCost
//       (RawCrop.replantCostNormal (unitProfit, uo, pa),
//        (fun amap a -> temp amap (farmingDist a)),
//        model.CropAmounts)

//   console.time "cached"
//   for _ = 1 to 1000 do
//     for crop in crops do
//       for fert in fertilizers do
//         let cached = rep fert crop 2 1
//         ignore cached
//   console.timeEnd "cached"

//   let deps = Model.rawCropUnitProfit model, Model.unitOutput model, Model.processorActiveOption model
//   let farmingDist = Model.farmingDistribution model |> memoize
//   let forageDist = Model.foragingAmounts model
//   let rep2 =
//     let temp =
//       Crop.replantCost
//         (RawCrop.replantCost deps,
//          RawCrop.replantCostNormal deps,
//          model.CropAmounts)
//         lowestPrice
//         forageDist
//     fun fert crop h s ->
//       temp (Fertilizer.qualityOption fert |> farmingDist) h s crop

//   console.time "full"
//   for _ = 1 to 1000 do
//     for crop in crops do
//       for fert in fertilizers do
//         let full = rep2 fert crop 2 1
//         ignore full
//   console.timeEnd "full"

let view ((ui, data) as model) dispatch =
  match ui.Page with
  | Home ->
      div [ ClassName "home" ]
        [ str "Select a mode:"
          nav []
            modes ]
          // button [ OnClick <| fun _ -> debug model ]
          //   [ str "Debug" ] ]
  | Mode mode ->
      div [ ClassName "app" ]
        [ div [ ClassName "top" ]
            [ span []
                [ str "Stardew Valley Stonks" ]
              nav []
                modes ]
          match mode with
          | Compare ->
              //what if there are no valid/profitable pairs -> display?
              let data = DataModel.combos AppCache.lowestPrice AppCache.unitProfit data

              let pairs = System.Collections.Generic.Dictionary()
              data |> Seq.iter (fun pair -> pairs.Add(pair.Description, pair))

              Re.responsiveContainer []
                [ Re.barChart
                    [ Re.Props.Chart.Data data ]
                    [ Re.yaxis [] []
                      Re.tooltip
                        [ Re.Props.Tooltip.Content (chartTooltip pairs.TryFind) ]
                        []
                      Re.bar
                        [ Re.Props.Cartesian.DataKey Combo.value
                          Re.Props.Cartesian.BarSize 40.0
                          Re.Props.Cell.Fill "blue" ]
                        []
                      Re.brush
                        [ Re.Props.Cartesian.DataKey Combo.description
                          Re.Props.Cartesian.StartIndex (float graphBrushStart)
                          Re.Props.Cartesian.EndIndex (min (data.Length - 1) graphBrushEnd |> float)
                          Re.Props.Cartesian.OnChange !!indexChange ]
                        []
                      Re.xaxis
                        [ Re.Props.Cartesian.DataKey Combo.description
                          Re.Props.Cartesian.Tick (pairImage pairs.TryFind)
                          Re.Props.Cartesian.Interval 0
                          Re.Props.Cartesian.Height 50.0 ]
                        [] ] ]

              // div []
              //   [ checkboxText "Show Unprofitable Combinations" ToggleShowUnprofitable model.ShowUnprofitable dispatch
              //     selectFertilizerOption (Model.activeFertilizers model) [ str "Select a Fertilizer" ] SetCompareFertilizer model.CompareFertilizer dispatch
              //     selectStringOptionWith "Any" (Model.activeCrops model) [ str "Select a Crop" ] SetCompareCrop model.CompareCrop dispatch ]
              
              // match model.SelectedCombo with
              // | Some (crop, fert) ->
              //     let valid =
              //       match model.CompareCrop with
              //       | Some c -> c = crop
              //       | None -> true
              //       &&
              //       match model.CompareFertilizer with
              //       | Some f -> f = fert
              //       | None -> true
              //       //can give one harvest
              //     if not valid then
              //       nothing
              //     else
              //       div []
              //         [ div []
              //             [ str <| ofName crop + " " + (defaultMap "None" ofName fert) ]
              //           match model.Crops.[crop] with
              //           | _ -> nothing ]
              // | None -> nothing
          | _ -> br []
          
          settings model dispatch
          ]

  | Help ->
      span []
        [ str "Help! I need somebody!" ]

//--App--
open Elmish
open Elmish.UrlParser
open Elmish.Navigation
#if DEBUG
open Elmish.HMR
open Elmish.Debug
#endif


Program.mkProgram AppData.initialModel Update.update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.toNavigable (parseHash Page.parseUrl) Update.urlUpdate
|> Program.withReactBatched "elmish-app"
|> Program.run
