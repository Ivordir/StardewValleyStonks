module StardewValleyStonks.WebApp.View.Settings

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update
open StardewValleyStonks.WebApp.View
open StardewValleyStonks.WebApp.View.Table

open Feliz
open Elmish.React

open type Html
open type prop
open type React

open Core.Operators
open Core.ExtraTopLevelOperators

let cropQualities qualities =
  let mapQualities (content: _ -> _ -> ReactElement) =
    div (qualities |> Qualities.indexed |> Array.map (fun (quality, prob) ->
      div [
        className (quality |> Quality.name |> lowerCase)
        style [ style.custom ("flexGrow", prob) ]
        children (content quality prob)
      ]
    ))

  div [ className Class.cropQualities; children [
    mapQualities (fun _ _ -> none)
    mapQualities (fun quality prob ->
      fragment [
        Html.span [ className Class.labelHidden; text (Quality.name quality) ]
        Html.span (if prob = 0.0 then "" else percent2 prob)
      ])
  ]]

let private payForFertilizerSettings profit dispatch =
  fragment [
    Input.checkbox
      "Pay for Fertilizer"
      profit.PayForFertilizer
      (SetPayForFertilizer >> dispatch)

    Html.span [
      if not profit.PayForFertilizer then className Class.disabled
      children [
        Input.checkbox
          "Pay for Destroyed Fertilizer"
          profit.PayForDestroyedFertilizer
          (SetPayForDestroyedFertilizer >> dispatch)
      ]
    ]
  ]

let viewPrice price =
  fragment [
    match price with
    | Some (source, price) ->
      ofNat price
      match source with
      | NonCustom vendor -> Icon.NoText.vendor vendor
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

let private customColumn
  viewValue
  (editValue: _ -> _ -> ReactElement)
  defaultValue
  (viewKey: _ -> ReactElement)
  key
  selection
  dispatch
  =
  Column.sortableOpt
    (ofStr "Custom")
    (fun item ->
      let key = key item
      let value = selection.Values.TryFind key
      fragment [
        value |> ofOption viewValue

        Dialog.toggleEdit
          (fragment [
            ofStr "Custom Price For "
            viewKey key
          ])
          (value |> Option.defaultValue defaultValue)
          (curry (if value.IsSome then EditCustom else AddCustom) key >> dispatch)
          (fun close v setValue ->
            div [
              editValue v setValue

              if value.IsSome then
                button [
                  className Class.button
                  onClick (fun _ -> RemoveCustom key |> dispatch; close ())
                  text "Delete"
                ]
            ])
      ])
    (key >> selection.Values.TryFind)
  |> Column.withSelect
      (fun key ->
        if selection.Values.ContainsKey key
        then Some (selection.Selected.Contains key)
        else None)
      (SelectCustom >> dispatch)

let private customPriceColumn viewKey key selection dispatch =
  customColumn
    ofNat
    (fun value setValue -> labeledHidden "Price" (Input.nat (length.em 3) value setValue))
    0u
    viewKey
    key
    selection
    dispatch

let private priceColumns prices selected custom priceValue icon vendors selectMsg customMsg dispatch =
  vendors |> Array.map (fun vendor ->
    Column.valueOptSortable
      (Icon.NoText.vendor vendor)
      (priceValue vendor)
      (ofOption ofNat)
    |> Column.withSelect
      (fun key ->
        if prices |> Table.find key |> Table.containsKey vendor
        then Some (selected |> Map.find key |> Set.contains vendor)
        else None)
      (curry selectMsg vendor >> dispatch))
  |> Array.insertEnd (customPriceColumn icon id custom (customMsg >> dispatch))

let private lazyTable table data settings sort items dispatch =
  lazyView2
    (fun (data, settings, sort, items) dispatch -> table data settings sort items dispatch)
    (data, settings, sort, items)
    dispatch

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

  let table data settings cropSort crops dispatch =
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
              then Icon.NoText.season season
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
      | ForageCrop crop -> crop.Foragables |> Array.insertStart (ForageCrop.seedItem crop))

    let items =
      Array.append nonForageItems forageItems
      |> Array.distinct
      |> Array.sortBy (data.Items.Find >> Item.name)

    let nonForageItems = Set.ofArray nonForageItems

    let productQuality = cropTab.ProductQuality
    let normalizedPrices = cropTab.NormalizeProductPrices

    fragment [
      div [ className Class.settingsGroup; children [
        labeled "Show Prices for Quality" (Select.enum (length.em 4) productQuality (SetProductQuality >> cropTabDispatch))
        Input.checkbox "Show Normalized Prices" normalizedPrices (SetNormalizeProductPrices >> cropTabDispatch)
      ]]

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
              (Icon.NoText.processor processor)
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
            (fun (price, preserveQuality) setState ->
              fragment [
                Input.nat (length.em 3) price (fun price -> setState (price, preserveQuality))
                |> labeledHidden "Price"

                Input.checkbox
                  "Scale with quality"
                  preserveQuality
                  (fun preserveQuality -> setState (price, preserveQuality))
              ])
            (0u, false)
            (Icon.itemId data)
            Item.id
            settings.Selected.CustomSellPrices
            (SetCustomSellPrice >> selectDispatch)
        |]
    ]

  let seeds data settings seedSort crops dispatch =
    let settingsDispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> settingsDispatch

    fragment [
      div [ className Class.settingsGroup; children [
        Select.unitUnion
          (length.em 5)
          settings.Profit.SeedStrategy
          (SetSeedStrategy >> SetProfit >> settingsDispatch)
        |> labeled "Seed Strategy"

        Input.checkbox
          "Joja Membership"
          settings.Game.JojaMembership
          (SetJojaMembership >> SetGameVariables >> settingsDispatch)
      ]]

      tableFromColumms
        id
        (crops |> Array.map Crop.seed)
        seedSort
        (SetSeedSort >> SetCropTabState >> SetUI >> dispatch)
        [|
          Column.sortable (ofStr "Seed") (Icon.seed data) (toItem >> data.Items.Find >> Item.name)
          |> Column.markAsKey

          yield!
            priceColumns
              data.SeedPrices
              settings.Selected.SeedPrices
              settings.Selected.CustomSeedPrices
              (Query.seedPriceValueFromVendor data settings)
              (Icon.seed data)
              (seedVendors data)
              SelectSeedPrices
              SetCustomSeedPrice
              selectDispatch

          Column.create (Icon.NoText.processor Processor.seedMaker) (konst none)
          |> Column.withSelect
            (fun seed ->
              if Crop.canGetOwnSeedsFromSeedMaker data.Items.Find data.Crops[seed]
              then Some (settings.Selected.UseSeedMaker.Contains seed)
              else None)
            (SelectUseSeedMaker >> selectDispatch)
          |> Column.withDisabled (not (Game.processorUnlocked data settings.Game Processor.seedMaker))

          Column.create (ofStr "Raw") (konst none)
          |> Column.withSelect
            (fun seed ->
              if Crop.makesOwnSeeds data.Crops[seed]
              then Some (settings.Selected.UseHarvestedSeeds.Contains seed)
              else None)
            (SelectUseHarvestedSeeds >> selectDispatch)

          Column.create (ofStr "Forage") (konst none)
          |> Column.withSelect
            (fun seed ->
              if Crop.isForage data.Crops[seed]
              then Some (settings.Selected.UseForageSeeds.Contains seed)
              else None)
            (SelectUseForageSeeds >> selectDispatch)
          |> Column.withDisabled (not (crops |> Seq.exists (function
            | ForageCrop crop -> ForageCrop.seedRecipeUnlocked settings.Game.Skills crop
            | FarmCrop _ -> false)))
        |]
    ]

  let private filteredCrops app =
    let data = app.Data
    let settings, ui = app.State
    let filters = ui.CropTab.Filters

    let optionFilter projection filterValue = filterValue |> Option.map (fun value -> projection >> (=) value)

    let nameFilter =
      if filters.ItemNameSearch = "" then None else
      let itemNameMatchesSearch name =
        data.Items[name]
        |> Item.name
        |> lowerCase
        |> strContains (lowerCase filters.ItemNameSearch)

      Some (fun crop ->
        crop |> Crop.seed |> toItem |> itemNameMatchesSearch
        || crop |> Crop.items |> Seq.exists itemNameMatchesSearch)

    let filters = List.choose id [
      nameFilter
      Some (if filters.InSeason then Game.cropIsInSeason settings.Game else Crop.growsInSeasons filters.Seasons)
      filters.Regrows |> optionFilter Crop.regrows
      filters.Giant |> optionFilter Crop.giant
      filters.Forage |> optionFilter Crop.isForage
    ]

    data.Crops.Values
    |> Seq.filter (fun crop -> filters |> Seq.forall (fun predicate -> predicate crop))
    |> Array.ofSeq

  let sortKey data = Crop.name data.Items.Find

  let private selectFilter name value dispatch =
    Select.options
      (length.em 2)
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

    fragment [
      input [
        className Class.inputBox
        placeholder "Search..."
        prop.type'.search
        prop.value filters.ItemNameSearch
        onChange (SetItemNameSearch >> dispatch)
      ]
      |> labeledHidden "Search"

      div [
        Input.checkbox "In Season" filters.InSeason (SetInSeason >> dispatch)
        yield! (Enum.values |> Array.map (fun season ->
          Html.span [
            if filters.InSeason then className Class.disabled
            children
              (Input.checkbox
                (Season.name season)
                (filters.Seasons |> Seasons.contains season)
                (toggleSeason season >> SetSeasons >> dispatch))
          ]
        ))
      ]

      div [
        selectFilter "Regrows" filters.Regrows (SetRegrows >> dispatch)
        selectFilter "Giant" filters.Giant (SetGiant >> dispatch)
        selectFilter "Forage" filters.Forage (SetForage >> dispatch)
        button [
          className Class.button
          onClick (fun _ -> dispatch ClearFilters)
          text "Clear"
        ]
      ]
    ]

  let tab app dispatch =
    let data = app.Data
    let settings, ui = app.State
    let cropTab = ui.CropTab
    let uiDispatch = SetUI >> dispatch
    let cropTabDispatch = SetCropTabState >> uiDispatch

    let crops = filteredCrops app |> Array.sortBy (sortKey data)

    fragment [
      detailsSection
        ui.OpenDetails
        OpenDetails.CropFilters
        (ofStr "Filters")
        (cropFilter cropTab.Filters (SetCropFilters >> cropTabDispatch))
        uiDispatch

      lazyDetails
        ui.OpenDetails
        OpenDetails.Crops
        (ofStr "Crops")
        (lazyTable table data settings cropTab.CropSort crops dispatch)
        uiDispatch

      lazyDetails
        ui.OpenDetails
        OpenDetails.Products
        (ofStr "Products")
        (lazyTable products data settings cropTab crops dispatch)
        uiDispatch

      lazyDetails
        ui.OpenDetails
        OpenDetails.Seeds
        (ofStr "Seeds")
        (lazyTable seeds data settings cropTab.SeedSort crops dispatch)
        uiDispatch
    ]


module Fertilizers =
  let fertilizerVendors = refMemo (fun (data: GameData) -> sortKeysByHighestCount data.FertilizerPrices)

  let table data settings fertSort fertilizers dispatch =
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SetSelections >> SetSettings >> dispatch

    fragment [
      div [ className Class.settingsGroup; children [
        Input.checkbox
          "Allow No Fertilizer"
          settings.Selected.NoFertilizer
          (SelectNoFertilizer >> selectDispatch)
      ]]

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

          Column.valueSortable (ofStr "Growth Rate") Fertilizer.speed (percent >> ofStr)

          Column.valueSortable (ofStr "Crop Qualities") Fertilizer.quality (fun fertQuality ->
            Skills.farmCropQualitiesFrom fertQuality settings.Game.Skills |> cropQualities)
        |]
    ]

  let prices (data: GameData) settings fertPriceSort fertilizers dispatch =
    let uiDispatch = SetUI >> dispatch
    let dispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> dispatch

    let fertilizers = fertilizers |> Array.map Fertilizer.name

    fragment [
      div [ className Class.settingsGroup; children (payForFertilizerSettings settings.Profit (SetProfit >> dispatch)) ]

      tableFromColumms
        id
        fertilizers
        fertPriceSort
        (SetFertilizerPriceSort >> uiDispatch)
        [|
          Column.valueSortable (ofStr "Fertilizer") id Icon.fertilizerName
          |> Column.markAsKey

          yield!
            priceColumns
              data.FertilizerPrices
              settings.Selected.FertilizerPrices
              settings.Selected.CustomFertilizerPrices
              (fun vendor -> data.FertilizerPrices.Find >> Table.tryFind vendor)
              Icon.fertilizerName
              (fertilizerVendors data)
              SelectFertilizerPrices
              SetCustomFertilizerPrice
              selectDispatch
        |]
    ]

  let tab app dispatch =
    let data = app.Data
    let settings, ui = app.State
    let uiDispatch = SetUI >> dispatch

    let fertilizers = data.Fertilizers.Values |> Seq.sortBy Fertilizer.name |> Array.ofSeq

    fragment [
      lazyDetails
        ui.OpenDetails
        OpenDetails.Fertilizers
        (ofStr "Fertilizers")
        (lazyTable table data settings ui.FertilizerSort fertilizers dispatch)
        uiDispatch

      lazyDetails
        ui.OpenDetails
        OpenDetails.FertilizerPrices
        (ofStr "Prices")
        (lazyTable prices data settings ui.FertilizerPriceSort fertilizers dispatch)
        uiDispatch
    ]


module Settings =
  let private date label (otherDate: Date) clamp (date: Date) dispatch =
    let clamp season day =
      if season = otherDate.Season
      then day |> clamp otherDate.Day
      else day

    div [ className Class.date; children [
      labeled $"{label} Date" none

      Select.enum
        (length.em 4)
        date.Season
        (fun season -> dispatch {
          Season = season
          Day = date.Day |> clamp season
        })
      |> labeledHidden $"{label} Season"

      Input.natWith
        (length.em 2)
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

  let profession skills profession dispatch =
    let selected = skills.Professions.Contains profession
    div [
      label [
        if not (skills |> Skills.professionUnlocked profession) then className Class.disabled
        children [
          input [
            prop.type'.checkbox
            isChecked selected
            onCheckedChange (curry SetProfession profession >> dispatch)
          ]
          Icon.profession profession
        ]
      ]
    ]

  let skillBuffLevel skill dispatch =
    div [ className Class.skillLevel; children [
      Input.natWithRange "Level" (length.em 2) 0u Skill.maxLevel skill.Level (SetLevel >> dispatch)
      labeled "Buff" (Input.nat (length.em 2) skill.Buff (SetBuff >> dispatch))
    ]]

  let skill skills skill professions qualities icon levelMsg dispatch =
    div [ className Class.skill; children [
      icon
      skillBuffLevel skill (levelMsg >> dispatch)
      div [
        className Class.professions
        children (professions |> Array.map (fun prof -> profession skills prof dispatch))
      ]
      cropQualities (qualities skills)
    ]]

  let farming skills dispatch =
    skill
      skills
      skills.Farming
      Profession.farmingProfessions
      Skills.farmCropQualities
      Icon.farming
      SetFarming
      dispatch

  let foraging skills dispatch =
    skill
      skills
      skills.Foraging
      Profession.foragingProfessions
      Skills.forageCropQualities
      Icon.foraging
      SetForaging
      dispatch

  let skills skills dispatch =
    fragment [
      farming skills dispatch

      foraging skills dispatch

      div [ className Class.settingsGroup; children [
        Input.checkbox
          "Ignore Skill Level Unlocks"
          skills.IgnoreSkillLevelRequirements
          (SetIgnoreSkillLevelRequirements >> dispatch)

        Input.checkbox
          "Ignore Profession Conflicts"
          skills.IgnoreProfessionConflicts
          (SetIgnoreProfessionConflicts >> dispatch)
      ]]
    ]

  let multipliers multipliers dispatch =
    fragment [
      Input.checkboxWith Icon.bearsKnowledge multipliers.BearsKnowledge (SetBearsKnowledge >> dispatch)

      Input.checkbox
        "Apply Tiller to Foraged Grapes and Blackberries"
        multipliers.TillerForForagedFruit
        (SetTillerForForagedFruit >> dispatch)

      Select.options
        (length.em 4)
        (fun margin -> ofStr (if margin = 1.0 then "Normal" else percent margin))
        [| 1.0..(-0.25)..0.25 |]
        multipliers.ProfitMargin
        (SetProfitMargin >> dispatch)
      |> labeled "Profit Margin"
    ]

  let profitSettings profit dispatch =
    fragment [
      Select.unitUnion
        (length.em 5)
        profit.SeedStrategy
        (SetSeedStrategy >> dispatch)
      |> labeled "Seed Strategy"

      payForFertilizerSettings profit dispatch
    ]

  let priceSettings game profit dispatch =
    fragment [
      Input.checkbox
        "Joja Membership"
        game.JojaMembership
        (SetJojaMembership >> SetGameVariables >> dispatch)

      multipliers game.Multipliers (SetMultipliers >> SetGameVariables >> dispatch)

      profitSettings profit (SetProfit >> dispatch)
    ]

  let cropSettings irrigated settings settingsDispatch =
    let dispatch = SetCropAmount >> settingsDispatch
    fragment [
      Input.checkbox "Irrigated" irrigated (SetIrrigated >> settingsDispatch)

      Input.floatWithRange
        "Average Giant Crop Orientations Per Tile"
        (length.em 4)
        10e-4
        CropAmount.minGiantCropOrientationsPerTile
        CropAmount.maxGiantCropOrientationsPerTile
        settings.GiantCropOrientationsPerTile
        (SetGiantCropOrientationsPerTile >> dispatch)

      Select.options
        (length.em 4)
        (Option.defaultOrMap "None" ToolLevel.name >> ofStr)
        (Enum.values |> Array.map Some |> Array.insertStart None)
        settings.ShavingToolLevel
        (SetShavingToolLevel >> dispatch)
      |> labeled "Shaving Enchantment"

      Input.checkboxWith Icon.specialCharm settings.SpecialCharm (SetSpecialCharm >> dispatch)

      Input.natWith (length.em 2) None (Some CropAmount.maxLuckBuff) settings.LuckBuff (SetLuckBuff >> dispatch)
      |> labelWith Class.labelText Icon.luckBuff
    ]

  let mods qualityArtisanProducts dispatch =
    Input.checkbox "Quality Artisan Products" qualityArtisanProducts (SetQualityArtisanProducts >> dispatch)

  let tab openDetails data game profit dispatch =
    let uiDispatch = SetUI >> dispatch
    let settingsDispatch = SetSettings >> dispatch
    let dispatch = SetGameVariables >> settingsDispatch

    fragment [
      div [ className Class.settingsGroup; children [
        dates game.StartDate game.EndDate dispatch
        labeled "Location" (Select.unitUnion (length.em 6) game.Location (SetLocation >> dispatch))
      ]]

      detailsSection
        openDetails
        OpenDetails.Skills
        (ofStr "Skills")
        (skills game.Skills (SetSkills >> dispatch))
        uiDispatch

      detailsSection
        openDetails
        OpenDetails.Multipliers
        (ofStr "Price Settings")
        (priceSettings game profit settingsDispatch)
        uiDispatch

      detailsSection
        openDetails
        OpenDetails.CropSettings
        (ofStr "Crop Settings")
        (cropSettings game.Irrigated game.CropAmount dispatch)
        uiDispatch

      detailsSection
        openDetails
        OpenDetails.Mod
        (ofStr "Mods")
        (mods game.QualityArtisanProducts dispatch)
        uiDispatch
    ]


module LoadSave =
  let viewPreset preset =
    div [ className Class.preset; children [
      Html.span preset.Name
      if preset.UniqueId.IsSome then
        let startDate = preset.Settings.Game.StartDate
        Html.span $"{Season.name startDate.Season} {startDate.Day}"
    ]]

  let saveFileInput save setSave =
    div [ className Class.fileInput; children [
      let loadFile (file: Browser.Types.File) =
        file.text().``then`` (fun text -> Some (file.name, Data.loadSaveGame text) |> setSave) |> ignore

      div [
        label [
          ofStr "Choose Save File"
          input [
            prop.type'.file
            onChange loadFile
            autoFocus true
          ]
        ]

        save |> ofOption viewPreset
      ]

      div [
        onDrop (fun e ->
          handleEvent e
          if e.dataTransfer.files.length > 0 then
            loadFile e.dataTransfer.files[0])

        onDragOver handleEvent
        text "or drag the file here"
      ]
    ]]

  let saveFileMessages presets save =
    ul [ className Class.messages; children [
      save |> ofOption (fun (fileName, preset) ->
        fragment [
          if fileName <> "SaveGameInfo" then
            li (Icon.warning $"""It appears you have chosen a file named "{fileName}". Please choose the file named "SaveGameInfo".""")

          match preset with
          | None -> li (Icon.error "Failed to load the save game.")
          | Some (preset, missing: string array) ->
            if preset.UniqueId |> Option.exists (fun uniqueId -> presets |> List.exists (Preset.hasId uniqueId)) then
              li (Icon.warning """This save game has been previously imported. Clicking "Ok" will update/overwrite the existing preset.""")

            if missing.Length > 0 then
              li (Icon.warningWith (fragment [
                Html.span "Warning: failed to load the following data from the save game:"
                ul (missing |> Array.map li)
                Html.span """Click "Ok" if you want to continue anyways."""
              ]))
        ])
    ]]

  let importSave presets dispatch =
    Dialog.toggleEditWith
      "Import Save"
      (ofStr "Import Save Game")
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

          saveFileInput (save |> Option.bind snd |> Option.map fst) setSave

          saveFileMessages presets save
        ])

  let nuclearReset dispatch =
    Dialog.confirmAction
      "Nuclear Reset"
      (Icon.warning "Nuclear Reset")
      (fun () -> dispatch NuclearReset)
      (p [
        ofStr "Warning: This will reset Stardew Valley Stonks to its default settings, "
        strong "deleting"
        ofStr " all custom presets in the process."
      ])

  let presetList presets loadDispatch saveDispatch =
    let presets =
      if presets |> List.isEmpty then
        [
          li [
            className Class.empty
            ariaHidden true
            text "No presets..."
          ]
        ]
      else
        presets |> List.mapi (fun i preset ->
          li [
            viewPreset preset

            div [
              button [
                className Class.button
                onClick (fun _ -> loadDispatch preset.Settings)
                text "Load"
              ]

              Dialog.toggleEditWith
                "Edit"
                (ofStr $"Rename Preset '{preset.Name}'")
                preset.Name
                (curry RenamePreset i >> saveDispatch)
                (fun _ name setName -> labeled "Name" (Input.text name setName))

              button [
                className Class.button
                onClick (fun _ -> DeletePreset i |> saveDispatch)
                text "Delete"
              ]
            ]
          ])

    div [
      Html.span "Presets"
      ul presets
    ]

  let tab presets settings dispatch =
    let saveDispatch = SetPresets >> dispatch
    let loadDispatch = LoadSettings >> SetState >> dispatch

    fragment [
      presetList presets loadDispatch saveDispatch

      div [
        Dialog.toggleEditWith
          "Save Current Settings"
          (ofStr "New Preset")
          "Untitled Preset"
          (fun name -> SavePreset (name, settings) |> saveDispatch)
          (fun _ name setName -> labeled "Name" (Input.text name setName))

        importSave presets saveDispatch
      ]

      div [
        button [
          className Class.button
          onClick (fun _ -> loadDispatch Data.defaultSettings)
          text "Reset Settings"
        ]

        nuclearReset dispatch
      ]
    ]

let section app dispatch =
  let appDispatch = dispatch
  let dispatch = SetState >> dispatch
  let settings, ui = app.State

  section [ prop.id "settings"; children [
    tabs "Settings" ui.SettingsTab (SetSettingsTab >> SetUI >> dispatch) (function
      | Crops -> Crops.tab app dispatch
      | Fertilizers -> Fertilizers.tab app dispatch
      | Misc -> Settings.tab ui.OpenDetails app.Data settings.Game settings.Profit dispatch
      | LoadSave -> LoadSave.tab app.Presets settings appDispatch)
  ]]
