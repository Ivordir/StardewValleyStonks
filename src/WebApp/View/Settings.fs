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
      className [
        Class.profession
        if not (skills |> Skills.professionUnlocked profession) then Class.disabled
      ]
      children [
        input [
          prop.type'.checkbox
          isChecked selected
          onCheckedChange (curry SetProfession profession >> dispatch)
        ]
        Icon.profession profession
      ]
    ]

  let private qualityClasses = Enum.values |> Array.map (Quality.name >> lowerCase >> className)

  let cropQualities qualities =
    let qualities = Qualities.toArray qualities
    let mapQualities (text: _ -> ReactElement) =
      (qualityClasses, qualities) ||> Array.map2 (fun class' prob ->
        div [
          class'
          style [ style.custom ("flexGrow", prob) ]
          children (text prob)
        ])

    div [ className Class.cropQualities; children [
      div [ className Class.cropQualitiesBars; children (mapQualities (konst none)) ]
      div [ className Class.cropQualitiesProbs; children (mapQualities (fun prob ->
        if prob = 0.0
        then none
        else ofStr (percent2 prob)))
      ]
    ]]

  let skillBuffLevel skill dispatch =
    div [ className Class.skillLevel; children [
      Html.span [
        labeled "Level" (Input.natWith (length.rem 2) None (Some Skill.maxLevel) skill.Level (SetLevel >> dispatch))
        labeledHidden "Level" (Input.natRange 0u Skill.maxLevel skill.Level (SetLevel >> dispatch))
      ]

      labeled "Buff" (Input.nat (length.rem 2) skill.Buff (SetBuff >> dispatch))
    ]]

  let farming skills dispatch =
    let farming = skills.Farming
    div [ className Class.skill; children [
      Icon.skill "Farming"
      skillBuffLevel farming (SetFarming >> dispatch)
      div [ className Class.professions; children [
        div (profession skills Tiller dispatch)
        div [
          profession skills Artisan dispatch
          profession skills Agriculturist dispatch
        ]
      ]]
      cropQualities (Skills.farmCropQualities skills)
    ]]

  let foraging skills dispatch =
    let foraging = skills.Foraging
    div [ className Class.skill; children [
      Icon.skill "Foraging"
      skillBuffLevel foraging (SetForaging >> dispatch)
      div [ className Class.professions; children [
        div (profession skills Gatherer dispatch)
        div (profession skills Botanist dispatch)
      ]]
      cropQualities (Skills.forageCropQualities skills)
    ]]

  let tab skills dispatch =
    fragment [
      farming skills dispatch
      foraging skills dispatch
      div [
        Input.checkboxText
          "Ignore Skill Level Unlocks"
          skills.IgnoreSkillLevelRequirements
          (SetIgnoreSkillLevelRequirements >> dispatch)

        Input.checkboxText
          "Ignore Profession Conflicts"
          skills.IgnoreProfessionConflicts
          (SetIgnoreProfessionConflicts >> dispatch)
      ]
    ]

let viewPrice price =
  fragment [
    match price with
    | Some (source, price) ->
      ofNat price
      match source with
      | NonCustom vendor -> Icon.vendorNoText vendor
      | Custom () -> none
    | None -> ofStr "???"
  ]

let private sortKeysByHighestCount table =
  table
  |> Table.values
  |> Seq.collect Table.keys
  |> Seq.countBy id
  |> Seq.sortByDescending snd
  |> Seq.map fst
  |> Array.ofSeq

let private customColumn viewValue editValue defaultValue title key selection dispatch =
  Column.sortableOpt
    (ofStr "Custom")
    (fun item ->
      let key = key item
      let value = selection.Values.TryFind key
      fragment [
        value |> ofOption viewValue

        Dialog.toggleEdit
          title
          (value |> Option.defaultValue defaultValue)
          (curry (if value.IsSome then EditCustom else AddCustom) key >> dispatch)
          (editValue key)
      ])
    (key >> selection.Values.TryFind)
  |> Column.withSelect
      (fun key ->
        if selection.Values.ContainsKey key
        then Some (selection.Selected.Contains key)
        else None)
      (SelectCustom >> dispatch)

let private customPriceColumn title key selection dispatch =
  let editPrice key close value setValue =
    fragment [
      Input.nat (length.rem 7.5) value setValue
      button [
        className Class.button
        onClick (fun _ -> dispatch (RemoveCustom key); close ())
        text "Delete"
      ]
    ]

  customColumn ofNat editPrice 0u title key selection dispatch

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

  let table (data: GameData) settings cropSort crops dispatch =
    let selectDispatch = SelectCrops >> SetSelections >> SetSettings >> dispatch

    tableFromColummsWithRowDisable
      (fun crop -> not (Query.canMakeEnoughSeeds data settings crop && Game.cropIsInSeason settings.Game crop))
      Crop.seed
      crops
      cropSort
      (SetCropSort >> SetCropTabState >> SetUI >> dispatch)
      [|
        Column.sortable (ofStr "Crop") (Icon.crop data) (Crop.name data.Items.Find)
        |> Column.withSelect (settings.Selected.Crops.Contains >> Some) selectDispatch
        |> Column.markAsKey

        Column.sortableOpt
          (ofStr "Seed Price")
          (Crop.seed >> Query.Price.seedMinVendorAndPrice data settings >> viewPrice)
          (Crop.seed >> Query.Price.seedMinPrice data settings)

        Column.valueSortable (ofStr "Days") (Game.growthTime settings.Game None) ofNat

        Column.valueOptSortable (ofStr "Regrow") Crop.regrowTime (ofOption ofNat)

        Column.createWith
          (ofStr "Seasons")
          (fun crop ->
            Html.span [ className Class.seasons; children (Enum.values |> Array.map (fun season ->
              if Crop.growsInSeason season crop
              then Icon.seasonNoText season
              else div []
            ))
          ])
          (Some (fun ascending ->
            let compare c1 c2 =
              match Crop.seasons c1, Crop.seasons c2 with
              | Seasons.None, Seasons.None -> 0
              | Seasons.None, _ -> 1
              | _, Seasons.None -> -1
              | s1, s2 -> Seasons.setOrder s1 s2
            if ascending
            then fun a b -> compare a b
            else fun a b -> compare b a))
      |]

  let products (data: GameData) settings cropTab crops dispatch =
    let cropTabDispatch = SetCropTabState >> SetUI >> dispatch
    let selectDispatch = SetSelections >> SetSettings >> dispatch

    let nonForageItems = crops |> Array.collect (function
      | FarmCrop crop -> FarmCrop.items crop
      | ForageCrop _ -> [||])

    let forageItems = crops |> Array.collect (function
      | FarmCrop _ -> [||]
      | ForageCrop crop -> Array.append [| ForageCrop.seedItem crop |] crop.Items)

    let items =
      Array.append nonForageItems forageItems
      |> Array.distinct
      |> Array.sortBy (data.Items.Find >> Item.name)

    let nonForageItems = Set.ofArray nonForageItems

    let productQuality = cropTab.ProductQuality
    let normalizedPrices = cropTab.NormalizeProductPrices

    fragment [
      labeled "View with quality" (Select.enum (length.rem 4) productQuality (SetProductQuality >> cropTabDispatch))

      Input.checkboxText "Normalize Prices" normalizedPrices (SetNormalizeProductPrices >> cropTabDispatch)

      let price =
        if normalizedPrices
        then Game.productNormalizedPrice data settings.Game productQuality
        else Game.productPrice data settings.Game productQuality >> float

      tableFromColumms
        Item.id
        (items |> Array.map data.Items.Find)
        cropTab.ProductSort
        (SetProductSort >> cropTabDispatch)
        [|
          Column.sortable (ofStr "Item") Icon.item Item.name |> Column.markAsKey

          Column.valueSortable
            (ofStr "Raw")
            (fun item ->
              Game.itemPrice settings.Game (nonForageItems |> Set.contains item.Id |> not) item productQuality)
            ofNat
          |> Column.withSelect
            (settings.Selected.SellRaw.Contains >> Some)
            (SelectSellRaw >> selectDispatch)

          yield! processors data |> Array.map (fun processor ->
            Column.valueOptSortable
              (Icon.processor processor)
              (Item.id >> GameData.product data processor >> Option.map price)
              (ofOption ofFloat)
            |> Column.withSelect
              (fun item ->
                if GameData.product data processor item |> Option.isSome
                then Some (settings.Selected.Products[item].Contains processor)
                else None)
              (curry SelectProducts processor >> selectDispatch)
            |> Column.withDisabled (not (Game.processorUnlocked data settings.Game processor)))

          customColumn
            (Query.customSellPriceValue productQuality >> ofNat)
            (fun _ _ (price, preserveQuality) setState ->
              fragment [
                Input.nat (length.rem 2) price (fun price -> setState (price, preserveQuality))
                Input.checkboxText
                  "Scale with quality"
                  preserveQuality
                  (fun preserveQuality -> setState (price, preserveQuality))
              ])
            (0u, false)
            "Custom Sell Price"
            Item.id
            settings.Selected.CustomSellPrices
            (SetCustomSellPrice >> selectDispatch)
        |]
    ]

  let seeds (data: GameData) settings seedSort crops dispatch =
    let settingsDispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> settingsDispatch

    fragment [
      Input.checkboxText
        "Joja Membership"
        settings.Game.JojaMembership
        (SetJojaMembership >> SetGameVariables >> settingsDispatch)

      Select.unitUnion
        (length.rem 5)
        settings.Profit.SeedStrategy
        (SetSeedStrategy >> SetProfit >> settingsDispatch)
      |> labeled "Seed Strategy"

      tableFromColumms
        Crop.seed
        crops
        seedSort
        (SetSeedSort >> SetCropTabState >> SetUI >> dispatch)
        [|
          Column.sortable (ofStr "Crop") (Icon.crop data) (Crop.name data.Items.Find)
          |> Column.markAsKey

          yield! seedVendors data |> Array.map (fun vendor ->
            Column.valueOptSortable
              (Icon.vendor vendor)
              (Crop.seed >> Query.seedPriceValueFromVendor data settings vendor)
              (ofOption ofNat)
            |> Column.withSelect
              (fun seed ->
                if data.SeedPrices[seed].ContainsKey vendor
                then Some (settings.Selected.SeedPrices[seed].Contains vendor)
                else None)
              (curry SelectSeedPrices vendor >> selectDispatch))

          customPriceColumn
            "Custom Seed Price"
            Crop.seed
            settings.Selected.CustomSeedPrices
            (SetCustomSeedPrice >> selectDispatch)

          Column.create (Icon.processor Processor.seedMaker) (konst none)
          |> Column.withSelect
            (fun seed ->
              if Crop.canGetOwnSeedsFromSeedMaker data.Crops[seed]
              then Some (settings.Selected.UseSeedMaker.Contains seed)
              else None)
            (SelectUseSeedMaker >> selectDispatch)
          |> Column.withDisabled (not (Game.processorUnlocked data settings.Game Processor.seedMaker))

          Column.create (ofStr "Raw Seeds") (konst none)
          |> Column.withSelect
            (fun seed ->
              if Crop.makesOwnSeeds data.Crops[seed]
              then Some (settings.Selected.UseHarvestedSeeds.Contains seed)
              else None)
            (SelectUseHarvestedSeeds >> selectDispatch)

          Column.create (ofStr "Forage Seeds") (konst none)
          |> Column.withSelect
            (fun seed ->
              if Crop.isForage data.Crops[seed]
              then Some (settings.Selected.UseForageSeeds.Contains seed)
              else None)
            (SelectUseForageSeeds >> selectDispatch)
          |> Column.withDisabled (crops |> Seq.exists (function
            | ForageCrop crop -> ForageCrop.seedRecipeUnlocked settings.Game.Skills crop
            | FarmCrop _ -> false) |> not)
        |]
    ]

  let private filteredCrops app =
    let data = app.Data
    let settings, ui = app.State
    let filters = ui.CropTab.Filters

    let optionFilter projection filterValue = filterValue |> Option.map (fun value -> projection >> (=) value)
    let filters = List.choose id [
      if filters.NameSearch = ""
      then None
      else Some (fun crop -> (crop |> Crop.name data.Items.Find |> lowerCase).Contains (lowerCase filters.NameSearch))
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
      (length.rem 2)
      (function
        | Some true -> ofStr "Yes"
        | Some false -> ofStr "No"
        | None -> ofStr "Any")
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
        className Class.inputBox
        placeholder "Search..."
        prop.type'.text
        prop.value filters.NameSearch
        onChange (SetNameSearch >> dispatch)
      ]

      div [
        Input.checkboxText "In Season" filters.InSeason (SetInSeason >> dispatch)
        Html.span [
          if filters.InSeason then className Class.disabled
          children (Enum.values |> Array.map (fun season ->
            Input.checkboxWith
              (Icon.season season)
              (filters.Seasons |> Seasons.contains season)
              (toggleSeason season >> SetSeasons >> dispatch)))
        ]
      ]

      selectFilter "Regrows" filters.Regrows (SetRegrows >> dispatch)
      selectFilter "Giant" filters.Giant (SetGiant >> dispatch)
      selectFilter "Forage" filters.Forage (SetForage >> dispatch)

      button [
        className Class.button
        onClick (fun _ -> dispatch ClearFilters)
        text "Clear"
      ]
    ]

  let tab app dispatch =
    let data = app.Data
    let settings, ui = app.State
    let cropTab = ui.CropTab
    let cropTabDispatch = SetCropTabState >> SetUI >> dispatch
    let crops = filteredCrops app |> Array.sortBy (sortKey data)

    fragment [
      cropFilter cropTab.Filters (SetCropFilters >> cropTabDispatch)

      tabs "Crop" cropTab.Tab (SetCropTab >> cropTabDispatch) (function
        | CropsTable -> table data settings cropTab.CropSort crops dispatch
        | ProductsTable -> products data settings cropTab crops dispatch
        | SeedsTable -> seeds data settings cropTab.SeedSort crops dispatch)
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
        Input.checkboxText "Allow No Fertilizer" settings.Selected.NoFertilizer (SelectNoFertilizer >> selectDispatch)

        tableFromColummsWithRowDisable
          (Fertilizer.name >> Query.fertilizerCost data settings >> Option.isNone)
          Fertilizer.name
          fertilizers
          fertSort
          (SetFertilizerSort >> uiDispatch)
          [|
            Column.valueSortable
              (ofStr "Fertilizer")
              Fertilizer.name
              Icon.fertilizerName
            |> Column.withSelect
              (settings.Selected.Fertilizers.Contains >> Some)
              (SelectFertilizers >> selectDispatch)
            |> Column.markAsKey


            Column.sortableOpt
              (ofStr "Price")
              (Fertilizer.name >> Query.Price.fertilizerMinVendorAndPrice data settings >> viewPrice)
              (Fertilizer.name >> Query.Price.fertilizerMinPrice data settings)

            Column.valueSortable (ofStr "Speed Bonus") Fertilizer.speed (percent >> ofStr)

            Column.valueSortable (ofStr "Crop Qualities") Fertilizer.quality (fun fertQuality ->
              Skills.farmCropQualitiesFrom fertQuality settings.Game.Skills |> Skills.cropQualities)
          |]
      ])
      (curry SetDetailsOpen OpenDetails.Fertilizers >> uiDispatch)

  let prices (data: GameData) settings fertPriceSort open' fertilizers dispatch =
    let uiDispatch = SetUI >> dispatch
    let dispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> dispatch

    let fertilizers = fertilizers |> Array.map Fertilizer.name

    animatedDetails
      open'
      (ofStr "Prices")
      (fragment [
        Input.checkboxText
          "Pay for Fertilizer"
          settings.Profit.PayForFertilizer
          (SetPayForFertilizer >> SetProfit >> dispatch)

        Html.span [
          if not settings.Profit.PayForFertilizer then className Class.disabled
          children [
            Input.checkboxText
              "Replace Lost Fertilizer"
              settings.Profit.ReplaceLostFertilizer
              (SetReplaceLostFertilizer >> SetProfit >> dispatch)
          ]
        ]

        tableFromColumms
          id
          fertilizers
          fertPriceSort
          (SetFertilizerPriceSort >> uiDispatch)
          [|
            Column.valueSortable (ofStr "Fertilizer") id Icon.fertilizerName
            |> Column.markAsKey

            yield! fertilizerVendors data |> Array.map (fun vendor ->
              Column.valueOptSortable
                (Icon.vendor vendor)
                (data.FertilizerPrices.Find >> Table.tryFind vendor)
                (ofOption ofNat)
              |> Column.withSelect
                (fun fertilizer ->
                  if data.FertilizerPrices[fertilizer].ContainsKey vendor
                  then Some (settings.Selected.FertilizerPrices[fertilizer].Contains vendor)
                  else None)
                (curry SelectFertilizerPrices vendor >> selectDispatch))

            customPriceColumn
              "Custom Fertilizer Price"
              id
              settings.Selected.CustomFertilizerPrices
              (SetCustomFertilizerPrice >> selectDispatch)
          |]
      ])
      (curry SetDetailsOpen OpenDetails.FertilizerPrices >> uiDispatch)

  let tab app dispatch =
    let data = app.Data
    let settings, ui = app.State
    let fertilizers = data.Fertilizers.Values |> Seq.sortBy Fertilizer.name |> Array.ofSeq
    fragment [
      table
        data
        settings
        ui.FertilizerSort
        (ui.OpenDetails.Contains OpenDetails.Fertilizers)
        fertilizers
        dispatch

      prices
        data
        settings
        ui.FertilizerPriceSort
        (ui.OpenDetails.Contains OpenDetails.FertilizerPrices)
        fertilizers
        dispatch
    ]


module Misc =
  let private date label (otherDate: Date) clamp (date: Date) dispatch =
    let clamp season day =
      if season = otherDate.Season
      then day |> clamp otherDate.Day
      else day

    div [ className Class.date; children [
      labeled $"{label} Date" none

      Select.enum
        (length.rem 4)
        date.Season
        (fun season -> dispatch {
          Season = season
          Day = date.Day |> clamp season
        })
      |> labeledHidden $"{label} Season"

      Input.natWith
        (length.rem 2)
        (Date.firstDay |> clamp date.Season |> Some)
        (Date.lastDay |> clamp date.Season |> Some)
        date.Day
        (fun day -> dispatch { date with Day = day })
      |> labeledHidden $"{label} Day"
    ]]

  let dates (startDate: Date) (endDate: Date) dispatch =
    fragment [
      date "Start" endDate min startDate (SetStartDate >> dispatch)
      date "End" startDate max endDate (SetEndDate >> dispatch)
    ]

  let multipliers multipliers dispatch =
    div [ className Class.settingsGroup; children [
      Input.checkboxText "Bear's Knowledge" multipliers.BearsKnowledge (SetBearsKnowledge >> dispatch)

      Select.options
        (length.rem 4)
        (fun margin -> ofStr (if margin = 1.0 then "Normal" else percent margin))
        [| 1.0..(-0.25)..0.25 |]
        multipliers.ProfitMargin
        (SetProfitMargin >> dispatch)
      |> labeled "Profit Margin"

      Input.checkboxText
        "Apply Tiller to Foraged Grapes and Blackberries"
        multipliers.TillerForForagedFruit
        (SetTillerForForagedFruit >> dispatch)
    ]]

  let cropAmountSettings settings dispatch =
    div [ className Class.settingsGroup; children [
      Html.span [
        Input.float
          (length.rem 4)
          10e-4
          (Some CropAmount.minPossibleGiantCropsPerTile)
          (Some CropAmount.maxPossibleGiantCropsPerTile)
          settings.PossibleGiantCropsPerTile
          (SetPossibleGiantCropsPerTile >> dispatch)
        |> labeled "Average Possible Giant Crops Per Tile"

        Input.floatRange
          10e-4
          CropAmount.minPossibleGiantCropsPerTile
          CropAmount.maxPossibleGiantCropsPerTile
          settings.PossibleGiantCropsPerTile
          (SetPossibleGiantCropsPerTile >> dispatch)
        |> labeledHidden "Average Possible Giant Crops Per Tile"
      ]

      Select.options
        (length.rem 4)
        (Option.defaultOrMap "None" ToolLevel.name >> ofStr)
        (Enum.values |> Array.map Some |> Array.append [| None |])
        settings.ShavingToolLevel
        (SetShavingToolLevel >> dispatch)
      |> labeled "Shaving Enchantment"

      Input.checkboxText "Special Charm" settings.SpecialCharm (SetSpecialCharm >> dispatch)

      Input.natWith (length.rem 2) None (Some CropAmount.maxLuckBuff) settings.LuckBuff (SetLuckBuff >> dispatch)
      |> labeled "Luck Buff"
    ]]

  let mods data open' modData dispatch =
    animatedDetails open'
      (ofStr "Mods")
      (fragment [
        let dispatch = SetModData >> SetGameVariables >> SetSettings >> dispatch
        Input.checkboxText "Quality Products" modData.QualityProducts (SetQualityProducts >> dispatch)
        ul [
          if not modData.QualityProducts then className Class.disabled
          children
            (Crops.processors data
            |> Array.filter ((<>) Processor.seedMaker)
            |> Array.map (fun processor ->
              li [
                Input.checkboxWith
                  (Icon.processor processor)
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
    fragment [
      div [ className Class.date; children [
        labeled "Location" (Select.unitUnion (length.rem 6) settings.Location (SetLocation >> dispatch))
        dates settings.StartDate settings.EndDate dispatch
      ]]
      multipliers settings.Multipliers (SetMultipliers >> dispatch)
      cropAmountSettings settings.CropAmount (SetCropAmount >> dispatch)
      div (Input.checkboxText "Irrigated" settings.Irrigated (SetIrrigated >> dispatch))
      mods data modsOpen settings.ModData appDispatch
    ]


module LoadSave =
  let saveFileInput setSave =
    div [
      let loadFile (file: Browser.Types.File) =
        file.text().``then`` (fun text -> Some (file.name, Data.loadSaveGame text) |> setSave) |> ignore

      label [ className Class.fileInput; children [
        ofStr "Choose Save File"
        input [
          prop.type'.file
          onChange loadFile
          autoFocus true
        ]
      ]]

      div [
        className Class.fileDropzone
        onDrop (fun e ->
          handleEvent e
          if e.dataTransfer.files.length > 0 then
            loadFile e.dataTransfer.files[0])

        onDragOver handleEvent
        text "or drag the file here"
      ]
    ]

  let saveFileMessages presets = ofOption (fun (fileName, preset) ->
    fragment [
      if fileName <> "SaveGameInfo" then
        ofStr $"It appears you have chosen a file named \"{fileName}\". Please choose the file named \"SaveGameInfo\"."

      match preset with
      | None -> ofStr "Failed to load the save game."
      | Some (preset, missing: string array) ->
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

  let importSave presets dispatch =
    Dialog.toggleEditWith
      "Import Save"
      "Import Save Game"
      None
      (Option.bind snd >> Option.iter (fst >> LoadSaveGame >> dispatch))
      (fun _ save setSave ->
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

          saveFileInput setSave
          saveFileMessages presets save
        ])

  let nuclearReset dispatch =
    Dialog.confirmAction
      "Nuclear Reset"
      "Nuclear Reset"
      (fun () -> dispatch NuclearReset)
      (p "Note: This will reset Stardew Valley Stonks to its default settings, deleting all custom presets in the process.")

  let tab app dispatch =
    let saveDispatch = SetPresets >> dispatch
    let loadDispatch = LoadSettings >> SetState >> dispatch
    fragment [
      ul (app.Presets |> List.mapi (fun i preset ->
        li [
          ofStr preset.Name

          button [
            className Class.button
            onClick (fun _ -> loadDispatch preset.Settings)
            text "Load"
          ]

          Dialog.toggleEditWith
            "Edit"
            "Rename"
            preset.Name
            (curry RenamePreset i >> saveDispatch)
            (konst Input.text)

          button [
            className Class.button
            onClick (fun _ -> saveDispatch (DeletePreset i))
            text "Delete"
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
          Dialog.toggleEditWith
            "Save Current Settings"
            "Save Current Settings As"
            "Untitled Settings"
            (fun name -> SavePreset (name, fst app.State) |> saveDispatch)
            (konst Input.text)

          importSave app.Presets saveDispatch

          button [
            className Class.button
            onClick (fun _ -> loadDispatch Data.defaultSettings)
            text "Reset Settings"
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
    tabs "Settings" ui.SettingsTab (SetSettingsTab >> SetUI >> dispatch) (function
      | Skills -> Skills.tab settings.Game.Skills (SetSkills >> SetGameVariables >> SetSettings >> dispatch)
      | Crops -> Crops.tab app dispatch
      | Fertilizers -> Fertilizers.tab app dispatch
      | Misc -> Misc.tab (ui.OpenDetails.Contains OpenDetails.Mod) app.Data settings.Game dispatch
      | SettingsTab.LoadSettings -> LoadSave.tab app appDispatch)
  ]]
