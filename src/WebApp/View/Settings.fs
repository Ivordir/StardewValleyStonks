module StardewValleyStonks.WebApp.View.Settings

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update
open StardewValleyStonks.WebApp.View
open StardewValleyStonks.WebApp.View.Table

open Feliz

open type Html
open type prop
open type React

open Core.Operators
open Core.ExtraTopLevelOperators

module Skills =
  let profession skills profession dispatch =
    let selected = skills.Professions.Contains profession
    label [
      classes [
        if selected then "active"
        if not (skills |> Skills.professionUnlocked profession) then "disabled"
      ]
      children [
        input [
          prop.type'.checkbox
          isChecked selected
          onCheckedChange (curry SetProfession profession >> dispatch)
        ]
        Image.Icon.profession profession
      ]
    ]

  let private qualityClasses =
    Qualities.init (fun quality -> className ((Quality.name quality).ToLower ()))

  let cropQualities (qualities: float Qualities) =
    div [ Class.cropQualities; children [
      div [ Class.cropQualitiesBars; children (Quality.all |> Array.map (fun quality ->
        div [
          qualityClasses[quality]
          style [ style.custom ("flexGrow", qualities[quality]) ]
        ]))
      ]

      div [ Class.cropQualitiesProbs; children (Quality.all |> Array.map (fun quality ->
        let prob = qualities[quality]
        div [
          qualityClasses[quality]
          style [ style.custom ("flexGrow", prob) ]
          if prob > 0.0 then text (percent2Decimal prob)
        ]))
      ]
    ]]

  let skill name skill dispatch =
    fragment [
      Html.span (Image.Icon.skill name)

      label [
        ofStr "Level:"
        Input.natWith (length.rem 2) None (Some Skill.maxLevel) skill.Level (SetLevel >> dispatch)
        Input.natRange 0u Skill.maxLevel skill.Level (SetLevel >> dispatch)
      ]

      label [
        ofStr "Buff:"
        Input.nat (length.rem 2) skill.Buff (SetBuff >> dispatch)
      ]
    ]

  let farming skills dispatch =
    let farming = skills.Farming
    div [
      skill "Farming" farming (SetFarming >> dispatch)
      div [ Class.professions; children [
        div (profession skills Tiller dispatch)
        div [
          profession skills Artisan dispatch
          profession skills Agriculturist dispatch
        ]
      ]]
      cropQualities (Skills.farmCropQualities skills)
    ]

  let foraging skills dispatch =
    let foraging = skills.Foraging
    div [
      skill "Foraging" foraging (SetForaging >> dispatch)
      div [ Class.professions; children [
        div (profession skills Gatherer dispatch)
        div (profession skills Botanist dispatch)
      ]]
      cropQualities (Skills.forageCropQualities skills)
    ]

  let tab skills dispatch =
    div [ Class.skills; children [
      farming skills dispatch
      foraging skills dispatch
      div [
        checkboxText "Ignore Skill Level Unlocks" skills.IgnoreSkillLevelRequirements (SetIgnoreSkillLevelRequirements >> dispatch)
        checkboxText "Ignore Profession Conflicts" skills.IgnoreProfessionConflicts (SetIgnoreProfessionConflicts >> dispatch)
      ]
    ]]

let viewPrice price =
  fragment [
    match price with
    | Some (source, price) ->
      ofNat price
      match source with
      | NonCustom vendor -> Image.vendor vendor
      | Custom () -> none
    | None -> ofStr "???"
  ]

let custom (viewValue: _ -> ReactElement) (editValue: _ -> _ -> ReactElement) defaultValue title (selection: Selection<_,_>) key dispatch =
  fragment [
    let value = selection.Values.TryFind key
    match value with
    | Some value ->
      checkbox (selection.Selected.Contains key) (curry SetSelected key >> SelectCustom >> dispatch)
      viewValue value
    | None -> none
    Dialog.toggleEdit
      title
      (if value.IsSome then "Edit" else "Add")
      (value |> Option.defaultValue defaultValue)
      (curry (if value.IsSome then EditCustom else AddCustom) key >> dispatch)
      editValue
  ]

let customPrice title selection key dispatch =
  custom ofNat (Input.nat (length.rem 7.5)) 0u title selection key dispatch


let private sortKeysByHighestCount table =
  table
  |> Table.values
  |> Seq.collect Table.keys
  |> Seq.countBy id
  |> Seq.sortByDescending snd
  |> Seq.map fst
  |> Array.ofSeq


module Crops =
  let seedVendors = refMemo (fun (data: GameData) -> sortKeysByHighestCount data.SeedPrices)

  let processors = refMemo (fun (data: GameData) ->
    data.Products.Values
    |> Seq.collect Table.keys
    |> Seq.append [|
      Processor.preservesJar
      Processor.keg
      Processor.seedMaker
    |]
    |> Seq.distinct
    |> Seq.sortWith (Option.noneMaxCompareBy data.ProcessorUnlockLevel.TryFind)
    |> Array.ofSeq)

  let table app cropSort crops dispatch =
    let data = app.Data
    let settings, _ = app.State
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SelectCrops >> SetSelections >> SetSettings >> dispatch
    sortTable [
      Column.header (columnCheckbox (crops |> Seq.map Crop.seed |> Set.ofSeq) settings.Selected.Crops selectDispatch)
      ofStr "Crop" |> Column.withSort (compareBy (Crop.name data.Items.Find))
      ofStr "Lowest Seed Price" |> Column.withSort (Option.noneMaxCompareBy (Crop.seed >> Query.Price.seedMinPrice data settings))
      ofStr "Growth Time" |> Column.withSort (compareBy (Game.growthTime settings.Game None))
      ofStr "Regrow Time" |> Column.withSort (Option.noneMaxCompareBy Crop.regrowTime)
      ofStr "Seasons" |> Column.withSort (fun c1 c2 ->
        match Crop.seasons c1, Crop.seasons c2 with
        | Seasons.None, Seasons.None -> 0
        | Seasons.None, _ -> 1
        | _, Seasons.None -> -1
        | s1, s2 -> Seasons.setOrder s1 s2)
    ]
      (fun crop ->
        let seed = Crop.seed crop
        let price = Query.Price.seedMinVendorAndPrice data settings seed
        let enoughSeeds = Query.canMakeEnoughSeeds data settings crop
        tr [
          key (string seed)
          if not enoughSeeds || not <| Game.cropIsInSeason settings.Game crop then Class.disabled
          children [
            td (checkbox (settings.Selected.Crops.Contains seed) (curry SetSelected seed >> selectDispatch))
            td (Image.Icon.crop data crop)
            td (viewPrice price)
            td (Game.growthTime settings.Game None crop |> ofNat)
            td (Crop.regrowTime crop |> Option.defaultOrMap none ofNat)
            td (Season.all |> Array.map (fun season ->
              Html.span [ Class.seasonSlot; children [
                if Crop.growsInSeason season crop then
                  Image.season season
              ]]
            ))
          ]
        ])
      (SetCropSort >> uiDispatch)
      cropSort
      crops

  let products data settings productSort productQuality showNormalizedPrices crops dispatch =
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SetSelections >> SetSettings >> dispatch

    let seedItemPairs = GameData.seedItemPairsFrom crops |> Set.ofArray
    let processors = processors data

    let itemRow mainCrop forage seed item =
      tr [
        td [
          if not mainCrop then
            ofStr "|__"
          Image.Icon.item' data item
        ]
        td [
          checkbox (settings.Selected.SellRaw.Contains (seed, item)) (curry SetSelected (seed, item) >> SelectSellRaw >> selectDispatch)
          ofNat <| Game.itemPrice settings.Game forage data.Items[item] productQuality
        ]
        fragment (processors |> Array.map (fun processor ->
          match GameData.product data item processor with
          | Some product ->
            td [
              if not <| Game.processorUnlocked data settings.Game processor then Class.disabled
              children [
                checkbox (settings.Selected.Products[seed, item].Contains processor) (curry SetSelected (seed, item) >> curry SelectProducts processor >> selectDispatch)
                if showNormalizedPrices
                then ofFloat <| Game.productNormalizedPrice data settings.Game productQuality product
                else ofNat <| Game.productPrice data settings.Game productQuality product
              ]
            ]
          | None -> td []
        ))
        td []
        td [
          custom
            (Query.customSellPriceValue productQuality >> ofNat)
            (fun (price, preserveQuality) setState ->
              fragment [
                Input.nat (length.rem 2) price (fun price -> setState (price, preserveQuality))
                checkboxText "Scale with quality" preserveQuality (fun preserveQuality -> setState (price, preserveQuality))
              ])
            (0u, false)
            "Custom Sell Price"
            settings.Selected.CustomSellPrices
            (seed, item)
            (SetCustomSellPrice >> selectDispatch)
        ]
      ]

    fragment [
      labeled "View with quality: " <| Select.options
        (length.rem 5)
        (Quality.name >> ofStr)
        Quality.all
        productQuality
        (SetProductQuality >> uiDispatch)

      checkboxText "Normalize Prices" showNormalizedPrices (SetShowNormalizedProductPrices >> uiDispatch)

      sortTable [
        ofStr "Crop" |> Column.withSort (compareBy (Crop.name data.Items.Find))

        Column.withSort
          (compareBy (fun crop -> Query.cropItemsHighestRawPrice data settings.Game crop productQuality))
          (fragment [
            columnCheckbox seedItemPairs settings.Selected.SellRaw (SelectSellRaw >> selectDispatch)
            ofStr "Raw Crop"
          ])

        let priceSort processor =
          if showNormalizedPrices
          then compareBy (fun crop -> Query.cropItemsHighestProductNormalizedPriceFrom data settings.Game crop productQuality processor)
          else compareBy (fun crop -> Query.cropItemsHighestProductPriceFrom data settings.Game crop productQuality processor)

        yield! processors |> Array.map (fun processor ->
          Column.withSort (priceSort processor) <| div [
            if not <| Game.processorUnlocked data settings.Game processor then Class.disabled
            let keys = seedItemPairs |> Set.filter (fun (_, item) -> GameData.product data item processor |> Option.isSome)
            children [
              checkbox
                (keys |> Set.forall (fun (seed, item) -> settings.Selected.Products[seed, item].Contains processor))
                (curry SetManySelected keys >> curry SelectProducts processor >> selectDispatch)
              Image.Icon.processor processor
            ]
          ])

        Column.withSort
          (compareBy (function
            | ForageCrop c -> Some (Game.seedItemSellPrice data settings.Game c.Seed)
            | FarmCrop _ -> None))
          (div [
            if not (data.ForageCrops.Values |> Seq.exists (ForageCrop.seedRecipeUnlocked settings.Game.Skills)) then Class.disabled
            children [
              columnCheckbox
                (crops |> Seq.choose (function | ForageCrop c -> Some c.Seed | FarmCrop _ -> None) |> Set.ofSeq)
                settings.Selected.SellForageSeeds
                (SelectSellForageSeeds >> selectDispatch)
              ofStr "Forage Seeds"
            ]
          ])

        Column.withSort
          (compareBy (fun crop -> Query.cropItemsHighestCustomPrice settings.Selected crop productQuality))
          (fragment [
            columnCheckbox
              (seedItemPairs |> Set.filter settings.Selected.CustomSellPrices.Values.ContainsKey)
              settings.Selected.CustomSellPrices.Selected
              (SelectCustom >> SetCustomSellPrice >> selectDispatch)
            ofStr "Custom"
          ])
      ]
        (fun crop ->
          let seed = Crop.seed crop
          keyedFragment (int seed, [
            match crop with
            | FarmCrop c ->
              itemRow true false seed c.Item
              match c.ExtraItem with
              | Some (item, _) -> itemRow false false seed item
              | None -> none
            | ForageCrop c ->
              tr [
                td (Image.Icon.crop data crop)
                fragment (Seq.replicate (processors.Length + 1) (td []))
                td [
                  if not <| ForageCrop.seedRecipeUnlocked settings.Game.Skills c then Class.disabled
                  children [
                    checkbox (settings.Selected.SellForageSeeds.Contains seed) (curry SetSelected seed >> SelectSellForageSeeds >> selectDispatch)
                    ofNat <| Game.seedItemSellPrice data settings.Game seed
                  ]
                ]
                td []
              ]
              c.Items |> Array.map (itemRow false true seed) |> fragment
          ]))
        (SetProductSort >> uiDispatch)
        productSort
        crops
    ]

  let seeds data settings seedSort crops dispatch =
    let uiDispatch = SetUI >> dispatch
    let settingsDispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> settingsDispatch

    let seedVendors = seedVendors data
    let seeds = crops |> Seq.map Crop.seed |> Set.ofSeq

    fragment [
      checkboxText "Joja Membership" settings.Game.JojaMembership (SetJojaMembership >> SetGameVariables >> settingsDispatch)
      labeled "Seed Strategy:" <| Select.unitUnion (length.rem 8) settings.Profit.SeedStrategy (SetSeedStrategy >> SetProfit >> settingsDispatch)

      sortTable [
        ofStr "Crop" |> Column.withSort (compareBy (Crop.name data.Items.Find))

        yield! seedVendors |> Array.map (fun vendor ->
          Column.withSort
            (Option.noneMaxCompareBy (Crop.seed >> Query.seedPriceValueFromVendor data settings vendor))
            (fragment [
              let keys = seeds |> Set.filter (data.SeedPrices.Find >> Table.containsKey vendor)
              checkbox
                (keys |> Set.forall (fun seed -> settings.Selected.SeedPrices[seed].Contains vendor))
                (curry SetManySelected keys >> curry SelectSeedPrices vendor >> selectDispatch)
              Image.Icon.vendor vendor
            ]))

        Column.header (div [
          if not <| Game.processorUnlocked data settings.Game Processor.seedMaker then Class.disabled
          children [
            columnCheckbox
              (crops |> Seq.choose (fun crop -> if Crop.canGetOwnSeedsFromSeedMaker crop then Some (Crop.seed crop) else None) |> Set.ofSeq)
              settings.Selected.UseSeedMaker
              (SelectUseSeedMaker >> selectDispatch)
            Image.Icon.processor Processor.seedMaker
          ]
        ])

        Column.header (fragment [
          columnCheckbox
            (crops |> Seq.choose (fun crop -> if Crop.makesOwnSeeds crop then Some (Crop.seed crop) else None) |> Set.ofSeq)
            settings.Selected.UseHarvestedSeeds
            (SelectUseHarvestedSeeds >> selectDispatch)
          ofStr "Raw Seeds"
        ])

        Column.header (div [
          if not (data.ForageCrops.Values |> Seq.exists (ForageCrop.seedRecipeUnlocked settings.Game.Skills)) then Class.disabled
          children [
            columnCheckbox
              (seeds |> Set.filter data.ForageCrops.ContainsKey)
              settings.Selected.UseForageSeeds
              (SelectUseForageSeeds >> selectDispatch)
            ofStr "Forage Seeds"
          ]
        ])

        Column.withSort
          (Option.noneMaxCompareBy (Crop.seed >> settings.Selected.CustomSeedPrices.Values.TryFind))
          (fragment [
            columnCheckbox
              (seeds |> Set.filter settings.Selected.CustomSeedPrices.Values.ContainsKey)
              settings.Selected.CustomSeedPrices.Selected
              (SelectCustom >> SetCustomSeedPrice >> selectDispatch)
            ofStr "Custom"
          ])
      ]
        (fun crop ->
          let seed = Crop.seed crop
          tr [ key (string seed); children [
            td (Image.Icon.crop data crop)
            fragment (seedVendors |> Array.map (fun vendor ->
              td [
                match data.SeedPrices[seed].TryFind vendor with
                | Some price ->
                  checkbox (settings.Selected.SeedPrices[seed].Contains vendor) (curry SetSelected seed >> curry SelectSeedPrices vendor >> selectDispatch)
                  Game.seedPrice data settings.Game seed price |> ofNat
                | None -> none
              ]
            ))
            td [
              if not <| Game.processorUnlocked data settings.Game Processor.seedMaker then Class.disabled
              children [
                if Crop.canGetOwnSeedsFromSeedMaker crop then
                  checkbox (settings.Selected.UseSeedMaker.Contains seed) (curry SetSelected seed >> SelectUseSeedMaker >> selectDispatch)
              ]
            ]
            td [
              if Crop.makesOwnSeeds crop then
                checkbox (settings.Selected.UseHarvestedSeeds.Contains seed) (curry SetSelected seed >> SelectUseHarvestedSeeds >> selectDispatch)
            ]
            match crop with
            | FarmCrop _ -> td []
            | ForageCrop c ->
              td [
                if not <| ForageCrop.seedRecipeUnlocked settings.Game.Skills c then Class.disabled
                children (checkbox (settings.Selected.UseForageSeeds.Contains seed) (curry SetSelected seed >> SelectUseForageSeeds >> selectDispatch))
              ]
            td [
              customPrice
                "Custom Seed Price"
                settings.Selected.CustomSeedPrices
                seed
                (SetCustomSeedPrice >> selectDispatch)
            ]
          ]])
        (SetSeedSort >> uiDispatch)
        seedSort
        crops
    ]

  let private filteredCrops app =
    let settings, ui = app.State
    let filters = ui.CropFilters
    let data = app.Data
    let optionFilter projection filterValue = filterValue |> Option.map (fun value -> projection >> (=) value)
    let filters = List.choose id [
      Some (fun crop -> (Crop.name data.Items.Find crop).ToLower().Contains (filters.NameSearch.ToLower()))
      Some (if filters.InSeason then Game.cropIsInSeason settings.Game else Crop.growsInSeasons filters.Seasons)
      filters.Regrows |> optionFilter Crop.regrows
      filters.Giant |> optionFilter Crop.giant
      filters.Forage |> optionFilter Crop.isForage
    ]
    data.Crops.Values
    |> Seq.filter (fun crop -> filters |> Seq.forall (fun predicate -> predicate crop))
    |> Array.ofSeq

  let sortKey (data: GameData) = Crop.name data.Items.Find

  let private selectFilter name value dispatch =
    Select.options
      (length.rem 3)
      (function
        | Some true -> "Yes"
        | Some false -> "No"
        | None -> "Any"
        >> ofStr)
      [| Some true; Some false; None |]
      value
      dispatch
    |> labeled name

  let cropFilter filters dispatch =
    let toggleSeason season selected =
      if selected
      then filters.Seasons |> Seasons.add season
      else filters.Seasons |> Seasons.remove season

    div [
      input [
        className "input-box"
        placeholder "Search..."
        prop.type'.text
        prop.value filters.NameSearch
        onChange (SetNameSearch >> dispatch)
      ]

      div [
        checkboxText "In Season" filters.InSeason (SetInSeason >> dispatch)
        Html.span [
          if filters.InSeason then Class.disabled
          children (Season.all |> Array.map (fun season ->
            checkboxWith
              (fragment [
                Image.season season
                ofStr <| Season.name season
              ])
              (filters.Seasons |> Seasons.contains season)
              (toggleSeason season >> SetSeasons >> dispatch)))
        ]
      ]

      selectFilter "Regrows" filters.Regrows (SetRegrows >> dispatch)
      selectFilter "Giant" filters.Giant (SetGiant >> dispatch)
      selectFilter "Forage" filters.Forage (SetForage >> dispatch)
      button [
        onClick (fun _ -> dispatch ClearFilters)
        text "Clear Filters"
      ]
    ]

  let tab app dispatch =
    let settings, ui = app.State
    let uiDispatch = SetUI >> dispatch
    let crops = filteredCrops app |> Seq.sortBy (sortKey app.Data)

    div [
      cropFilter ui.CropFilters (SetCropFilters >> uiDispatch)

      viewTabs ui.CropTab (SetCropTab >> uiDispatch)

      match ui.CropTab with
      | CropsTable -> table app ui.CropSort crops dispatch
      | ProductsTable -> products app.Data settings ui.ProductSort ui.ProductQuality ui.ShowNormalizedProductPrices crops dispatch
      | SeedsTable -> seeds app.Data settings ui.SeedSort crops dispatch
    ]


module Fertilizers =
  let fertilizerVendors = refMemo (fun (data: GameData) -> sortKeysByHighestCount data.FertilizerPrices)

  let table (data: GameData) settings fertSort open' fertilizers dispatch =
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SetSelections >> SetSettings >> dispatch
    animatedDetails
      open'
      (ofStr "Fertilizers")
      (fragment [
        checkboxText "Allow No Fertilizer" settings.Selected.NoFertilizer (SelectNoFertilizer >> selectDispatch)
        sortTable [
          Column.header (columnCheckbox (Set.ofSeq data.Fertilizers.Keys) settings.Selected.Fertilizers (SelectFertilizers >> selectDispatch))
          ofStr "Fertilizer" |> Column.withSort (compareBy Fertilizer.name)
          ofStr "Lowest Price" |> Column.withSort (Option.noneMaxCompareBy (Fertilizer.name >> Query.Price.fertilizerMinPrice data settings))
          ofStr "Speed Bonus"|> Column.withSort (compareBy Fertilizer.speed)
          ofStr "Crop Qualities" |> Column.withSort (compareBy Fertilizer.quality)
        ]
          (fun fertilizer ->
            let name = Fertilizer.name fertilizer
            let price = Query.Price.fertilizerMinVendorAndPrice data settings name
            tr [
              key (string name)
              if settings.Profit.PayForFertilizer && price = None then Class.disabled
              children [
                td (checkbox (settings.Selected.Fertilizers.Contains name) (curry SetSelected name >> SelectFertilizers >> selectDispatch))
                td (Image.Icon.fertilizer fertilizer)
                td (viewPrice price)
                td (fertilizer.Speed |> percent |> ofStr)
                td (Skills.farmCropQualitiesWith (Some fertilizer) settings.Game.Skills |> Skills.cropQualities)
              ]
            ])
          (SetFertilizerSort >> uiDispatch)
          fertSort
          fertilizers
      ])
      (curry SetDetailsOpen OpenDetails.Fertilizers >> uiDispatch)

  let prices (data: GameData) settings fertPriceSort open' fertilizers dispatch =
    let uiDispatch = SetUI >> dispatch
    let dispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> dispatch

    let fertilizerVendors = fertilizerVendors data
    let keys = Set.ofSeq data.Fertilizers.Keys

    animatedDetails
      open'
      (ofStr "Prices")
      (fragment [
        checkboxText "Pay for Fertilizer" settings.Profit.PayForFertilizer (SetPayForFertilizer >> SetProfit >> dispatch)
        Html.span [
          if not settings.Profit.PayForFertilizer then Class.disabled
          children [
            checkboxText
              "Replace Lost Fertilizer"
              settings.Profit.ReplaceLostFertilizer
              (SetReplaceLostFertilizer >> SetProfit >> dispatch)
          ]
        ]

        sortTable [
          ofStr "Fertilizer" |> Column.withSort (compareBy Fertilizer.name)

          yield! fertilizerVendors |> Array.map (fun vendor ->
            Column.withSort
              (Option.noneMaxCompareBy (Fertilizer.name >> data.FertilizerPrices.Find >> Table.tryFind vendor))
              (fragment [
                let keys = keys |> Set.filter (data.FertilizerPrices.Find >> Table.containsKey vendor)
                checkbox
                  (keys |> Set.forall (fun key -> settings.Selected.FertilizerPrices[key].Contains vendor))
                  (curry SetManySelected keys >> curry SelectFertilizerPrices vendor >> selectDispatch)
                Image.Icon.vendor vendor
              ]))

          Column.withSort
            (Option.noneMaxCompareBy (Fertilizer.name >> settings.Selected.CustomFertilizerPrices.Values.TryFind))
            (fragment [
              columnCheckbox
                (keys |> Set.filter settings.Selected.CustomFertilizerPrices.Values.ContainsKey)
                settings.Selected.CustomFertilizerPrices.Selected
                (SelectCustom >> SetCustomFertilizerPrice >> selectDispatch)
              ofStr "Custom"
            ])
        ]
          (fun fert ->
            let name = fert.Name
            tr [ key (string name); children [
              td (Image.Icon.fertilizer fert)
              fragment (fertilizerVendors |> Array.map (fun vendor ->
                td [
                  match data.FertilizerPrices[name].TryFind vendor with
                  | Some price ->
                    checkbox (settings.Selected.FertilizerPrices[name].Contains vendor) (curry SetSelected name >> curry SelectFertilizerPrices vendor >> selectDispatch)
                    ofNat price
                  | None -> none
                ]
              ))
              td [
                customPrice
                  "Custom Fertilizer Price"
                  settings.Selected.CustomFertilizerPrices
                  name
                  (SetCustomFertilizerPrice >> selectDispatch)
              ]
            ]])
          (SetFertilizerPriceSort >> uiDispatch)
          fertPriceSort
          fertilizers
      ])
      (curry SetDetailsOpen OpenDetails.FertilizerPrices >> uiDispatch)

  let tab app dispatch =
    let settings, ui = app.State
    let fertilizers = app.Data.Fertilizers.Values |> Seq.sortBy Fertilizer.name |> Array.ofSeq
    div [ prop.id "fertilizer-tab"; children [
      table app.Data settings ui.FertilizerSort (ui.OpenDetails.Contains OpenDetails.Fertilizers) fertilizers dispatch
      prices app.Data settings ui.FertilizerPriceSort (ui.OpenDetails.Contains OpenDetails.FertilizerPrices) fertilizers dispatch
    ]]


module Misc =
  let private date min max (date: Date) dispatch =
    div [ Class.date; children [
      Select.options
        (length.rem 6)
        (Season.name >> ofStr)
        Season.all
        date.Season
        (fun season -> dispatch { date with Season = season })

      Input.natWith
        (length.rem 2)
        (Some min)
        (Some max)
        date.Day
        (fun day -> dispatch { date with Day = day })
    ]]

  let dates (startDate: Date) (endDate: Date) dispatch =
    let sameSeason = startDate.Season = endDate.Season
    fragment [
      date Date.firstDay (if sameSeason then endDate.Day else Date.lastDay) startDate (SetStartDate >> dispatch)
      date (if sameSeason then startDate.Day else Date.firstDay) Date.lastDay endDate (SetEndDate >> dispatch)
    ]

  let multipliers multipliers dispatch =
    div [
      checkboxText "Bear's Knowledge" multipliers.BearsKnowledge (SetBearsKnowledge >> dispatch)
      labeled "Profit Margin:" <| Select.options
        (length.rem 5)
        (function
          | 1.0 -> ofStr "Normal"
          | margin -> ofStr (percent margin))
        [| 1.0..(-0.25)..0.25 |]
        multipliers.ProfitMargin
        (SetProfitMargin >> dispatch)

      checkboxText "Apply Tiller to Foraged Grapes and Blackberries" multipliers.TillerForForagedFruit (SetTillerForForagedFruit >> dispatch)
    ]

  let cropAmountSettings settings dispatch =
    div [
      label [
        ofStr "Giant Crop Checks Per Tile:"
        Input.floatWith
          (length.rem 4)
          10e-4
          (Some CropAmount.minGiantCropChecks)
          (Some CropAmount.maxGiantCropChecks)
          settings.GiantChecksPerTile
          (SetGiantChecksPerTile >> dispatch)

        Input.floatRange
          10e-4
          CropAmount.minGiantCropChecks
          CropAmount.maxGiantCropChecks
          settings.GiantChecksPerTile
          (SetGiantChecksPerTile >> dispatch)
      ]

      Select.options
        (length.rem 5)
        (Option.defaultOrMap "None" ToolLevel.name >> ofStr)
        (ToolLevel.all |> Array.map Some |> Array.append [| None |])
        settings.ShavingToolLevel
        (SetShavingToolLevel >> dispatch)
      |> labeled "Shaving Enchantment: "

      checkboxText "Special Charm" settings.SpecialCharm (SetSpecialCharm >> dispatch)
      label [
        ofStr "Luck Buff:"
        Input.natWith (length.rem 2) None (Some CropAmount.maxLuckBuff) settings.LuckBuff (SetLuckBuff >> dispatch)
      ]
    ]

  let mods data open' modData dispatch =
    animatedDetails open'
      (ofStr "Mods")
      (fragment [
        let dispatch = SetModData >> SetGameVariables >> SetSettings >> dispatch
        checkboxText "Quality Products" modData.QualityProducts (SetQualityProducts >> dispatch)
        ul [
          if not modData.QualityProducts then Class.disabled
          children (Crops.processors data |> Array.filter ((<>) Processor.seedMaker) |> Array.map (fun processor ->
            li [
              checkboxWith
                (Image.Icon.processor processor)
                (modData.QualityProcessors |> Set.contains processor)
                (curry SetQualityProcessors processor >> dispatch)
            ]
          ))
        ]
      ])
      (curry SetDetailsOpen OpenDetails.Mod >> SetUI >> dispatch)

  let tab modsOpen data settings dispatch =
    let appDispatch = dispatch
    let dispatch = SetGameVariables >> SetSettings >> dispatch
    div [ prop.id "misc"; children [
      div [ Class.date; children [
        labeled "Location: " <| Select.unitUnion (length.rem 7.5) settings.Location (SetLocation >> dispatch)
        dates settings.StartDate settings.EndDate dispatch
      ]]
      multipliers settings.Multipliers (SetMultipliers >> dispatch)
      cropAmountSettings settings.CropAmount (SetCropAmount >> dispatch)
      div (checkboxText "Irrigated" settings.Irrigated (SetIrrigated >> dispatch))
      mods data modsOpen settings.ModData appDispatch
    ]]


module LoadSave =
  let importSave presets dispatch =
    Dialog.toggleEdit
      "Import Save Game"
      "Import Save"
      None
      (Option.bind snd >> Option.iter (fst >> LoadSaveGame >> dispatch))
      (fun save setSave ->
        fragment [
          p [
            ofStr """
              Please provide the "SaveGameInfo" file for the save you want to import.
              Instructions for where to find this file can be found on the
            """
            a [
              href "https://stardewvalleywiki.com/Saves#Save_format"
              target "_blank"
              rel "noopener noreferrer"
              text "Stardew Valley Wiki"
            ]
            ofStr "."
          ]

          div [
            let loadFile (file: Browser.Types.File) =
              file.text().``then`` (fun text -> Some (file.name, Data.loadSaveGame text) |> setSave) |> ignore

            label [ Class.fileInput; children [
              ofStr "Choose Save File"
              input [
                prop.type'.file
                onChange loadFile
              ]
            ]]

            div [
              className "file-dropzone"
              onDrop (fun e ->
                handleEvent e
                if e.dataTransfer.files.length > 0 then
                  loadFile e.dataTransfer.files[0])

              onDragOver handleEvent
              text "or drag file here"
            ]
          ]

          match save with
          | None -> none
          | Some (fileName, preset) ->
            if fileName <> "SaveGameInfo" then
              ofStr $"It appears you have chosen a file named \"{fileName}\". Please choose the file named \"SaveGameInfo\"."

            match preset with
            | None -> ofStr "Failed to load the save game."
            | Some (preset, missing) ->
              div [
                Html.span preset.Name
                let startDate = preset.Settings.Game.StartDate
                Html.span $"{Season.name startDate.Season} {startDate.Day}"
              ]

              if missing.Length > 0 then
                ofStr "Warning: failed to load the following data from the save game:"
                ul (missing |> Array.map li)
                ofStr "Click \"Ok\" if you want to continue anyways."

              if preset.UniqueId |> Option.exists (fun uniqueId -> presets |> List.exists (Preset.hasId uniqueId)) then
                ofStr "This save game has been previously imported. Clicking \"Ok\" will update/overwrite the existing preset."
        ])

  let nuclearReset dispatch =
    Dialog.toggleEdit "Nuclear Reset" "Nuclear Reset" () (fun () -> dispatch NuclearReset) (fun () _ ->
      p "Note: This will reset Stardew Valley Stonks to its default settings, deleting all custom presets in the process.")

  let tab app dispatch =
    let saveDispatch = SetPresets >> dispatch
    let loadDispatch = LoadSettings >> SetState >> dispatch
    div [
      ul (app.Presets |> List.mapi (fun i preset ->
        li [
          ofStr preset.Name
          button [
            onClick (fun _ -> loadDispatch preset.Settings)
            text "Load"
          ]
          Dialog.toggleEdit "Rename" "Edit" preset.Name (curry RenamePreset i >> saveDispatch) Input.text
          button [
            onClick (fun _ -> saveDispatch (DeletePreset i))
            text "x"
          ]
        ]
      ))

      div [
        style [
          style.display.flex
          style.flexDirection.column
          style.width.maxContent
        ]
        children [
          Dialog.toggleEdit "Save Current Settings As" "Save Current Settings" "Untitled Settings" (fun name -> SavePreset (name, fst app.State) |> saveDispatch) Input.text
          importSave app.Presets saveDispatch

          button [
            onClick (fun _ -> loadDispatch Data.defaultSettings)
            text "Reset Settings to Default"
          ]

          nuclearReset dispatch
        ]
      ]
    ]

let section app dispatch =
  let appDispatch = dispatch
  let dispatch = SetState >> dispatch
  let settings, ui = app.State
  section [ prop.id "settings"; children [
    viewTabs ui.SettingsTab (SetSettingsTab >> SetUI >> dispatch)
    match ui.SettingsTab with
    | Skills -> Skills.tab settings.Game.Skills (SetSkills >> SetGameVariables >> SetSettings >> dispatch)
    | Crops -> Crops.tab app dispatch
    | Fertilizers -> Fertilizers.tab app dispatch
    | Misc -> Misc.tab (ui.OpenDetails.Contains OpenDetails.Mod) app.Data settings.Game dispatch
    | SettingsTab.LoadSettings -> LoadSave.tab app appDispatch
  ]]
