module StardewValleyStonks.WebApp.View.Settings

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update
open StardewValleyStonks.WebApp.View
open StardewValleyStonks.WebApp.View.Table

open Fable.Core
open Fable.Core.JsInterop
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
        ] ))
      ]

      div [ Class.cropQualitiesProbs; children (Quality.all |> Array.map (fun quality ->
        let prob = qualities[quality]
        div [
          qualityClasses[quality]
          style [ style.custom ("flexGrow", prob) ]
          if prob > 0.0 then text (percent2 prob)
        ] ))
      ]
    ] ]

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
      ] ]
      cropQualities (Skills.farmCropQualities skills)
    ]

  let foraging skills dispatch =
    let foraging = skills.Foraging
    div [
      skill "Foraging" foraging (SetForaging >> dispatch)
      div [ Class.professions; children [
        div (profession skills Gatherer dispatch)
        div (profession skills Botanist dispatch)
      ] ]
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
    ] ]

let viewPrice price =
  fragment [
    match price with
    | Some (NonCustom vendor, price) ->
      ofNat price
      Image.vendor vendor
    | Some (Custom (), price) -> ofNat price
    | None -> ofStr "???"
  ]

type 'a EditProps = {|
  Value: 'a option
  CloseModal: unit -> unit
  Dispatch: 'a -> unit
|}

let [<ReactComponent>] EditCustom (props: {|
    Existing: 'a option
    Edit: 'a EditProps -> ReactElement
    Dispatch: 'a -> unit
  |}) =
  let modal, setModal = useState false

  let modalRef = useRef<Browser.Types.HTMLDialogElement option> None

  useLayoutEffect (fun () ->
    match modalRef.current with
    | Some m when not m.``open`` -> m.showModal ()
    | _ -> ())

  fragment [
    button [
      onClick (fun _ -> setModal true)
      text (match props.Existing with | Some _ -> "Edit" | None -> "Add")
    ]

    if modal then
      dialog [
        Interop.mkAttr "onClose" (fun _ -> setModal false)
        prop.ref modalRef
        children (props.Edit {|
          Value = props.Existing
          CloseModal = fun () -> setModal false
          Dispatch = props.Dispatch
        |})
      ]
  ]

let [<ReactComponent>] EditCustomPrice (props: nat EditProps) =
  let price, setPrice = useState (props.Value |> Option.defaultValue 0u)
  fragment [
    h1 "Custom Price"
    Input.nat (length.rem 2) price setPrice
    div [
      button [
        onClick (fun _ -> props.Dispatch price; props.CloseModal ())
        text "Ok"
      ]
      button [
        onClick (fun _ -> props.CloseModal ())
        text "Cancel"
      ]
    ]
  ]

let viewCustom (viewValue: _ -> ReactElement) editValue (selection: Selection<_,_>) key dispatch =
  fragment [
    let value = selection.Values.TryFind key
    match value with
    | Some value ->
      checkbox (selection.Selected.Contains key) (curry SetSelected key >> SelectCustom >> dispatch)
      viewValue value
    | None -> none
    EditCustom {|
      Existing = value
      Edit = editValue
      Dispatch = (curry SetCustom key >> dispatch)
    |}
  ]

let viewCustomPrice selection key dispatch =
  viewCustom ofNat EditCustomPrice selection key dispatch

let [<ReactComponent>] EditCustomSellPrice (props: (nat * bool) EditProps) =
  let (price, preserveQuality), setState = useState (props.Value |> Option.defaultValue (0u, false))
  fragment [
    h1 "Custom Price"
    Input.nat (length.rem 2) price (fun price -> setState (price, preserveQuality))
    checkboxText "Scale with quality" preserveQuality (fun preserveQuality -> setState (price, preserveQuality))
    div [
      button [
        onClick (fun _ -> props.Dispatch (price, preserveQuality); props.CloseModal ())
        text "Ok"
      ]
      button [
        onClick (fun _ -> props.CloseModal ())
        text "Cancel"
      ]
    ]
  ]

let private sortKeysByHighestCount table =
  table
  |> Table.values
  |> Seq.collect Table.keys
  |> Seq.countBy id
  |> Seq.sortByDescending snd
  |> Seq.map fst
  |> Array.ofSeq


// default crop sort
module Crops =
  let seedVendors = refMemo (fun (data: GameData) -> sortKeysByHighestCount data.SeedPrices)

  let processors = refMemo (fun (data: GameData) ->
    data.Products.Values
    |> Seq.collect Table.keys
    |> Seq.append [|
      Processor.keg
      Processor.preservesJar
      Processor.seedMaker
    |]
    |> Seq.distinct
    |> Seq.sortWith (Option.noneMaxCompareBy data.ProcessorUnlockLevel.TryFind)
    |> Array.ofSeq)

  let table app cropSort crops dispatch =
    let data = app.Data
    let settings = app.State.Settings
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SelectCrops >> SetSelections >> SetSettings >> dispatch
    [
      sortTable [
        {
          Header = checkbox (data.Crops.Keys |> Seq.forall settings.Selected.Crops.Contains) (curry SetManySelected (crops |> Seq.map Crop.seed |> set) >> selectDispatch)
          Width = 0
          Sort = None
        }
        {
          Header = ofStr "Crop"
          Width = 40
          Sort = Some (compareBy (Crop.name data.Items.Find))
        }
        {
          Header = ofStr "Lowest Seed Price"
          Width = 25
          Sort = Some (Option.noneMaxCompareBy (Crop.seed >> Query.Price.seedMinPrice data settings))
        }
        {
          Header = ofStr "Growth Time"
          Width = 10
          Sort = Some (compareBy (Game.growthTime settings.Game None))
        }
        {
          Header = ofStr "Regrow Time"
          Width = 10
          Sort = Some (Option.noneMaxCompareBy Crop.regrowTime)
        }
        {
          Header = ofStr "Seasons"
          Width = 15
          Sort =
            Some (fun c1 c2 ->
              match Crop.seasons c1, Crop.seasons c2 with
              | Seasons.None, Seasons.None -> 0
              | Seasons.None, _ -> 1
              | _, Seasons.None -> -1
              | s1, s2 -> Seasons.setOrder s1 s2)
        }
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
                ] ]
              ))
            ]
          ] )
        (SetCropSort >> uiDispatch)
        cropSort
        crops
    ]

  let products data settings productSort productQuality showNormalizedPrices crops dispatch =
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SetSelections >> SetSettings >> dispatch

    let keys =
      crops
      |> Seq.collect (fun crop ->
        let seed = Crop.seed crop
        Crop.items crop |> Array.map (fun item -> seed, item))
      |> set

    let selectMany filter = keys |> Seq.filter filter |> set |> curry SetManySelected

    let processors = processors data
    let processorUnlocked = processors |> Array.map (Game.processorUnlocked data settings.Game.Skills)

    let viewItemRow mainCrop forage seed item =
      let theItem = data.Items[item]
      let selected = settings.Selected.Products[seed, item]
      tr [
        td [
          if not mainCrop then
            ofStr "|__"
          Image.Icon.item' data item
        ]
        td [
          checkbox (settings.Selected.SellRaw.Contains (seed, item)) (curry SetSelected (seed, item) >> SelectSellRaw >> selectDispatch)
          ofNat <| Game.itemPrice settings.Game forage theItem productQuality
        ]
        yield! processors |> Array.mapi (fun i processor ->
          match GameData.product data item processor with
          | Some product ->
            td [
              if not processorUnlocked[i] then Class.disabled
              children [
                checkbox (selected.Contains processor) (curry SetSelected (seed, item) >> curry SelectProducts processor >> selectDispatch)
                if showNormalizedPrices
                then ofFloat <| Game.productNormalizedPrice data settings.Game item productQuality product
                else ofNat <| Game.productPrice data settings.Game item productQuality product
              ]
            ]
          | None -> td [] )
        td []
        td [
          viewCustom
            (Query.customSellPriceValue productQuality >> ofNat)
            EditCustomSellPrice
            settings.Selected.CustomSellPrices
            (seed, item)
            (SetCustomSellPrice >> selectDispatch)
        ]
      ]

    let keyColWidth = 0.4
    let productWidth = 100.0 * (1.0 - keyColWidth) / float (processors.Length + 2)

    [
      labeled "View with quality: " <| Select.options
        (length.rem 5)
        (Quality.name >> ofStr)
        Quality.all
        productQuality
        (SetProductQuality >> uiDispatch)

      checkboxText "Normalize Prices" showNormalizedPrices (SetShowNormalizedProductPrices >> uiDispatch)

      sortTable [
        {
          Header = ofStr "Crop"
          Width = 100.0 * keyColWidth
          Sort = Some <| compareBy (Crop.name data.Items.Find)
        }
        {
          Header = fragment [
            checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (data.Crops
                  |> Table.toSeq
                  |> Seq.collect (fun (seed, crop) ->
                    Crop.items crop |> Array.map (fun item -> seed, item))
                  |> Seq.forall settings.Selected.SellRaw.Contains)
                (selectMany (konst true) >> SelectSellRaw >> selectDispatch)
            ofStr "Raw Crop"
          ]
          Width = productWidth
          Sort = Some <| compareByRev (fun crop -> Query.cropItemsHighestRawPrice data settings.Game crop productQuality)
        }
        yield! processors |> Array.mapi (fun i processor ->
          {
            Header =
              div [
                if not processorUnlocked[i] then Class.disabled
                children [
                  checkboxWith
                    [ onClick (fun e -> e.stopPropagation ()) ]
                    none
                    (settings.Selected.Products |> Map.forall (fun (_, item) selected -> selected.Contains processor || GameData.product data item processor |> Option.isNone))
                    (selectMany (fun (_, item) -> GameData.product data item processor |> Option.isSome) >> curry SelectProducts processor >> selectDispatch)
                  Image.Icon.processor processor
                ]
              ]
            Width = productWidth
            Sort = Some (
              if showNormalizedPrices
              then compareByRev (fun crop -> Query.cropItemsHighestProductNormalizedPriceFrom data settings.Game crop productQuality processor)
              else compareByRev (fun crop -> Query.cropItemsHighestProductPriceFrom data settings.Game crop productQuality processor))
          } )
        {
          Header =
            div [
              if not (data.ForageCrops.Values |> Seq.exists (ForageCrop.seedRecipeUnlocked settings.Game.Skills)) then Class.disabled
              children [
                checkboxWith
                  [ onClick (fun e -> e.stopPropagation ()) ]
                  none
                  (data.ForageCrops.Keys |> Seq.forall settings.Selected.SellForageSeeds.Contains)
                  (curry SetManySelected (crops |> Seq.filter Crop.isForage |> Seq.map Crop.seed |> set) >> SelectSellForageSeeds >> selectDispatch)
                ofStr "Forage Seeds"
              ]
            ]
          Width = productWidth
          Sort = Some <| compareByRev (fun crop -> if Crop.isForage crop then Some <| Game.itemPrice settings.Game true data.Items[Crop.seed crop * 1u<_>] Quality.Normal else None)
        }
        { Header = fragment [
            if settings.Selected.CustomSellPrices.Values.IsEmpty then
              none
            else
              checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (Selection.allSelected settings.Selected.CustomSellPrices)
                (selectMany settings.Selected.CustomSellPrices.Values.ContainsKey >> SelectCustom >> SetCustomSellPrice >> selectDispatch)
            ofStr "Custom"
          ]
          Width = 0
          Sort = Some <| compareByRev (fun crop -> Query.cropItemsHighestCustomPrice settings.Selected crop productQuality)
        }
      ]
        (fun crop ->
          let seed = Crop.seed crop
          keyedFragment (int seed, [
            match crop with
            | FarmCrop c ->
              viewItemRow true false seed c.Item
              match c.ExtraItem with
              | Some (item, _) -> viewItemRow false false seed item
              | None -> none
            | ForageCrop c ->
              tr [
                td (Image.Icon.crop data crop)
                yield! Seq.replicate (processors.Length + 1) (td [])
                td [
                  if not <| ForageCrop.seedRecipeUnlocked settings.Game.Skills c then Class.disabled
                  children [
                    checkbox (settings.Selected.SellForageSeeds.Contains seed) (curry SetSelected seed >> SelectSellForageSeeds >> selectDispatch)
                    ofNat <| Game.itemPrice settings.Game true data.Items[seed * 1u<_>] Quality.Normal
                  ]
                ]
                td []
              ]
              yield! c.Items |> Array.map (viewItemRow false true seed)
          ] ))
        (SetProductSort >> uiDispatch)
        productSort
        crops
    ]

  let seeds data settings seedSort crops dispatch =
    let uiDispatch = SetUI >> dispatch
    let settingsDispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> settingsDispatch

    let selectMany filter = curry SetManySelected (crops |> Seq.filter filter |> Seq.map Crop.seed |> set)

    let seedVendors = seedVendors data

    [
      checkboxText "Joja Membership" settings.Game.JojaMembership (SetJojaMembership >> SetGameVariables >> settingsDispatch)
      labeled "Seed Strategy:" <| Select.unitUnion (length.rem 8) settings.Profit.SeedStrategy (SetSeedStrategy >> SetProfit >> settingsDispatch)

      let keyColWdith = 0.4
      let width = 100.0 * ((1.0 - keyColWdith) / float seedVendors.Length) // div by 0?
      sortTable [
        {
          Header = ofStr "Crop"
          Width = 100.0 * keyColWdith
          Sort = Some <| compareBy (Crop.name data.Items.Find)
        }
        yield! seedVendors |> Array.map (fun vendor ->
          {
            Header = fragment [
              checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (settings.Selected.SeedPrices |> Map.forall (fun key selected -> selected |> Set.contains vendor || not (data.SeedPrices[key].ContainsKey vendor)))
                (selectMany (Crop.seed >> data.SeedPrices.Find >> Table.containsKey vendor) >> curry SelectSeedPrices vendor >> selectDispatch)
              Image.Icon.vendor vendor
            ]
            Width = width
            Sort = Some <| Option.noneMaxCompareBy (Crop.seed >> Query.seedPriceValueFromVendor  data settings vendor)
          } )
        {
          Header =
            div [
              if not <| Game.processorUnlocked data settings.Game.Skills Processor.seedMaker then Class.disabled
              children [
                checkbox
                  (data.Crops |> Table.forall (fun seed crop ->
                    settings.Selected.UseSeedMaker.Contains seed
                    || not <| Crop.canGetOwnSeedsFromSeedMaker crop))
                  (selectMany (fun crop -> Crop.canGetOwnSeedsFromSeedMaker crop) >> SelectUseSeedMaker >> selectDispatch)
                Image.Icon.processor Processor.seedMaker
              ]
            ]
          Width = 0
          Sort = None
        }
        {
          Header = fragment [
            checkbox
              (data.Crops |> Table.forall (fun seed crop -> not <| Crop.makesOwnSeeds crop || settings.Selected.UseHarvestedSeeds.Contains seed))
              (selectMany Crop.makesOwnSeeds >> SelectUseHarvestedSeeds >> selectDispatch)
            ofStr "Raw Seeds"
          ]
          Width = 0
          Sort = None
        }
        {
          Header =
            div [
              if not (data.ForageCrops.Values |> Seq.exists (ForageCrop.seedRecipeUnlocked settings.Game.Skills)) then Class.disabled
              children [
                checkbox
                  (data.Crops.Values
                    |> Seq.filter Crop.isForage
                    |> Seq.map Crop.seed
                    |> Seq.forall settings.Selected.UseForageSeeds.Contains)
                  (selectMany Crop.isForage >> SelectUseForageSeeds >> selectDispatch)
                ofStr "Forage Seeds"
              ]
            ]
          Width = 0
          Sort = None
        }
        {
          Header = fragment [
            if settings.Selected.CustomSeedPrices.Values.IsEmpty then
              none
            else
              checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (Selection.allSelected settings.Selected.CustomSeedPrices)
                (selectMany (Crop.seed >> settings.Selected.CustomSeedPrices.Values.ContainsKey) >> SelectCustom >> SetCustomSeedPrice >> selectDispatch)
            ofStr "Custom"
          ]
          Width = 0
          Sort = Some <| Option.noneMaxCompareBy (Crop.seed >> settings.Selected.CustomSeedPrices.Values.TryFind)
        }
      ]
        (fun crop ->
          let seed = Crop.seed crop
          tr [ key (string seed); children [
            td (Image.Icon.crop data crop)
            yield! seedVendors |> Array.map (fun vendor ->
              td [
                match data.SeedPrices[seed].TryFind vendor with
                | Some price ->
                  yield checkbox (settings.Selected.SeedPrices[seed].Contains vendor) (curry SetSelected seed >> curry SelectSeedPrices vendor >> selectDispatch)
                  yield Game.seedPrice data settings.Game seed price |> ofNat
                | None -> yield none
              ] )
            td [
              if not <| Game.processorUnlocked data settings.Game.Skills Processor.seedMaker then Class.disabled
              children [
                if Crop.canGetOwnSeedsFromSeedMaker crop then
                  checkbox (settings.Selected.UseSeedMaker.Contains seed) (curry SetSelected seed >> SelectUseSeedMaker >> selectDispatch)
              ]
            ]
            td [
              if Crop.makesOwnSeeds crop then
                checkbox (settings.Selected.UseHarvestedSeeds.Contains seed) (curry SetSelected seed >> SelectUseHarvestedSeeds >> selectDispatch)
            ]
            td (
              match crop with
              | FarmCrop _ -> []
              | ForageCrop c -> [
                if not <| ForageCrop.seedRecipeUnlocked settings.Game.Skills c then Class.disabled
                children (checkbox (settings.Selected.UseForageSeeds.Contains seed) (curry SetSelected seed >> SelectUseForageSeeds >> selectDispatch))
              ]
            )
            td [
              viewCustomPrice settings.Selected.CustomSeedPrices seed (SetCustomSeedPrice >> selectDispatch)
            ]
          ] ] )
        (SetSeedSort >> uiDispatch)
        seedSort
        crops
    ]

  let filteredCrops app =
    let filters = app.State.UI.CropFilters
    let data = app.Data
    let settings = app.State.Settings
    let optionFilter projection filterValue = filterValue |> Option.map (fun value -> projection >> (=) value) |> Option.toList
    let filters = [
      if filters.InSeason then Game.cropIsInSeason settings.Game else Crop.growsInSeasons filters.Seasons
      yield! filters.Regrows |> optionFilter Crop.regrows
      yield! filters.Giant |> optionFilter Crop.giant
      yield! filters.Forage |> optionFilter Crop.isForage
    ]
    data.Crops.Values
    |> Seq.filter (fun crop -> filters |> Seq.forall (fun predicate -> predicate crop))
    |> Array.ofSeq

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
      div [
        checkboxText "In Season" filters.InSeason (SetInSeason >> dispatch)
        Html.span [
          if filters.InSeason then Class.disabled
          children (Season.all |> Array.map (fun season ->
          checkboxWith []
            (fragment [
              Image.season season
              ofStr <| Season.name season ])
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
    let { UI = ui; Settings = settings } = app.State
    let crops = filteredCrops app

    let uiDispatch = SetUI >> dispatch

    div [
      cropFilter ui.CropFilters (SetCropFilters >> uiDispatch)

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.Crops)
        (ofStr "Crops")
        (table app ui.CropSort crops dispatch)
        (curry SetDetailsOpen OpenDetails.Crops >> uiDispatch)

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.Products)
        (ofStr "Products")
        (products app.Data settings ui.ProductSort ui.ProductQuality ui.ShowNormalizedProductPrices crops dispatch)
        (curry SetDetailsOpen OpenDetails.Products >> uiDispatch)

      animatedDetails
        (ui.OpenDetails.Contains OpenDetails.SeedSources)
        (ofStr "Seeds")
        (seeds app.Data settings ui.SeedSort crops dispatch)
        (curry SetDetailsOpen OpenDetails.SeedSources >> uiDispatch)
    ]


module Fertilizers =
  let fertilizerVendors = refMemo (fun (data: GameData) -> sortKeysByHighestCount data.FertilizerPrices)

  let table (data: GameData) settings fertSort open' dispatch =
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SetSelections >> SetSettings >> dispatch
    animatedDetails
      open'
      (ofStr "Fertilizers")
      [
        checkboxText "Allow No Fertilizer" settings.Selected.NoFertilizer (SelectNoFertilizer >> selectDispatch)
        sortTable [
          {
            Header =
              checkbox
                (data.Fertilizers.Keys |> Seq.forall settings.Selected.Fertilizers.Contains)
                (curry SetManySelected (set data.Fertilizers.Keys) >> SelectFertilizers >> selectDispatch)
            Width = 0
            Sort = None
          }
          {
            Header = ofStr "Fertilizer"
            Width = 40
            Sort = Some <| compareBy Fertilizer.name
          }
          {
            Header = ofStr "Lowest Price"
            Width = 20
            Sort = Some <| Option.noneMaxCompareBy (Fertilizer.name >> Query.Price.fertilizerMinPrice data settings)
          }
          {
            Header = ofStr "Speed Bonus"
            Width = 10
            Sort = Some <| compareBy Fertilizer.speed
          }
          {
            Header = ofStr "Crop Qualities"
            Width = 30
            Sort = Some <| compareBy Fertilizer.quality
          }
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
            ] )
          (SetFertilizerSort >> uiDispatch)
          fertSort
          data.Fertilizers.Values
      ]
      (curry SetDetailsOpen OpenDetails.Fertilizers >> uiDispatch)

  let prices (data: GameData) settings fertPriceSort open' dispatch =
    let uiDispatch = SetUI >> dispatch
    let dispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> dispatch

    let selectMany filter = curry SetManySelected (data.Fertilizers.Keys |> Seq.filter filter |> set)
    let fertilizerVendors = fertilizerVendors data

    animatedDetails
      open'
      (ofStr "Prices")
      [
        checkboxText "Pay for Fertilizer" settings.Profit.PayForFertilizer (SetPayForFertilizer >> SetProfit >> dispatch)
        checkboxWith [
          classes [
            "checkbox-label"
            if not settings.Profit.PayForFertilizer then "disabled"
          ]]
          (ofStr "Replace Lost Fertilizer")
          settings.Profit.ReplaceLostFertilizer
          (SetReplaceLostFertilizer >> SetProfit >> dispatch)

        let keyColWdith = 0.40
        let width = 100.0 * (1.0 - keyColWdith) / float fertilizerVendors.Length // div by 0?
        sortTable [
          {
            Header = ofStr "Fertilizer"
            Width = 100.0 * keyColWdith
            Sort = Some <| compareBy Fertilizer.name
          }
          yield! fertilizerVendors |> Array.map (fun vendor ->
            {
              Header = fragment [
                checkboxWith
                  [ onClick (fun e -> e.stopPropagation ()) ]
                  none
                  (settings.Selected.FertilizerPrices |> Map.forall (fun key selected -> selected |> Set.contains vendor || not (data.FertilizerPrices[key].ContainsKey vendor)))
                  (selectMany (data.FertilizerPrices.Find >> Table.containsKey vendor) >> curry SelectFertilizerPrices vendor >> selectDispatch)
                Image.Icon.vendor vendor
              ]
              Width = width
              Sort = Some <| Option.noneMaxCompareBy (Fertilizer.name >> data.FertilizerPrices.Find >> Table.tryFind vendor)
            } )
          {
            Header = fragment [
              if settings.Selected.CustomFertilizerPrices.Values.IsEmpty then
                none
              else
                checkboxWith
                  [ onClick (fun e -> e.stopPropagation ()) ]
                  none
                  (Selection.allSelected settings.Selected.CustomFertilizerPrices)
                  (selectMany settings.Selected.CustomFertilizerPrices.Values.ContainsKey >> SelectCustom >> SetCustomFertilizerPrice >> selectDispatch)
              ofStr "Custom"
            ]
            Width = 0
            Sort = Some <| Option.noneMaxCompareBy (Fertilizer.name >> settings.Selected.CustomFertilizerPrices.Values.TryFind)
          }
        ]
          (fun fert ->
            let name = fert.Name
            tr [ key (string name); children [
              td (Image.Icon.fertilizer fert)
              yield! fertilizerVendors |> Array.map (fun vendor ->
                td [
                  match data.FertilizerPrices[name].TryFind vendor with
                  | Some price ->
                    yield checkbox (settings.Selected.FertilizerPrices[name].Contains vendor) (curry SetSelected name >> curry SelectFertilizerPrices vendor >> selectDispatch)
                    yield ofNat price
                  | None -> yield none
                ] )
              td [
                viewCustomPrice settings.Selected.CustomFertilizerPrices name (SetCustomFertilizerPrice >> selectDispatch)
              ]
            ] ] )
          (SetFertilizerPriceSort >> uiDispatch)
          fertPriceSort
          data.Fertilizers.Values
      ]
      (curry SetDetailsOpen OpenDetails.FertilizerPrices >> uiDispatch)

  let tab app dispatch =
    let { UI = ui; Settings = settings } = app.State
    div [ prop.id "fertilizer-tab"; children [
      table app.Data settings ui.FertilizerSort (ui.OpenDetails.Contains OpenDetails.Fertilizers) dispatch
      prices app.Data settings ui.FertilizerPriceSort (ui.OpenDetails.Contains OpenDetails.FertilizerPrices) dispatch
    ] ]

module Misc =
  let date message (date: Date) dispatch =
    let dispatch = message >> dispatch
    div [ Class.date; children [
      Select.options
        (length.rem 6)
        (Season.name >> ofStr)
        Season.all
        date.Season
        (fun season -> dispatch { date with Season = season })
      Input.natWith (length.rem 2) (Some Date.firstDay) (Some Date.lastDay) date.Day (fun day -> dispatch { date with Day = day })
    ] ]

  let multipliers multipliers dispatch =
    div [
      checkboxText "Bear's Knowledge" multipliers.BearsKnowledge (SetBearsKnowledge >> dispatch)
      labeled "Profit Margin:" <| Select.options
        (length.rem 5)
        (function
          | 1.0 -> "Normal"
          | margin -> percent margin
          >> ofStr)
        [| yield! seq { 1.0..(-0.25)..0.25 } |]
        multipliers.ProfitMargin
        (SetProfitMargin >> dispatch)

      checkboxText "Apply Tiller to Foraged Grapes and Blackberries" multipliers.TillerForForagedFruit (SetTillerForForagedFruit >> dispatch)
    ]

  let cropAmountSettings settings dispatch =
    div [
      label [
        ofStr "Giant Crop Checks Per Tile:"
        Input.floatWith (length.rem 4) 10e4 (Some CropAmount.minGiantCropChecks) (Some CropAmount.maxGiantCropChecks) settings.GiantChecksPerTile (SetGiantChecksPerTile >> dispatch)
        Input.floatRange 10e4 CropAmount.minGiantCropChecks CropAmount.maxGiantCropChecks settings.GiantChecksPerTile (SetGiantChecksPerTile >> dispatch)
      ]
      // giant crop prob
        // P(not Giant) = (1.0 - baseGiantProb) ^ giantCropsPerTile
        // P(Giant) = 1.0 - P(not Giant)
      // avg yield from giant crop / tile
        // (minYield, maxYield) -> avg yield
        // + crops from shaving
        // / 9 tiles

      Select.options
        (length.rem 5)
        (Option.defaultOrMap "None" ToolLevel.name >> ofStr)
        [| None; yield! ToolLevel.all |> Array.map Some |]
        settings.ShavingToolLevel
        (SetShavingToolLevel >> dispatch)
      |> labeled "Shaving Enchantment: "

      checkboxText "Special Charm" settings.SpecialCharm (SetSpecialCharm >> dispatch)
      label [
        ofStr "Luck Buff:"
        Input.natWith (length.rem 2) None (Some CropAmount.maxLuckBuff) settings.LuckBuff (SetLuckBuff >> dispatch)
      ]
      // doubleCrop Chance
    ]

  let mods data open' modData dispatch =
    animatedDetails
      open'
      (ofStr "Mods")
      [
        let dispatch = SetModData >> SetGameVariables >> SetSettings >> dispatch
        checkboxText "Quality Products" modData.QualityProducts (SetQualityProducts >> dispatch)
        ul [
          if not modData.QualityProducts then Class.disabled
          children (Crops.processors data |> Array.filter ((<>) Processor.seedMaker) |> Array.map (fun processor ->
            li [
              checkboxWith []
                (Image.Icon.processor processor)
                (modData.QualityProcessors |> Set.contains processor)
                (curry SetQualityProcessors processor >> dispatch)
            ]
          ))
        ]
      ]
      (curry SetDetailsOpen OpenDetails.Mod >> SetUI >> dispatch)

  let tab modsOpen data settings dispatch =
    let appDispatch = dispatch
    let dispatch = SetGameVariables >> SetSettings >> dispatch
    div [ prop.id "misc"; children [
      div [ Class.date; children [
        // labeled "Location: " <| Select.unitUnion settings.Location (SetLocation >> dispatch)
        labeled "Location: " <| Select.unitUnion
          (length.rem 7.5)
          settings.Location
          (SetLocation >> dispatch)

        // check that not: (startSeason = endSeason and endDay < startday)
        date SetStartDate settings.StartDate dispatch
        date SetEndDate settings.EndDate dispatch
      ] ]

      multipliers settings.Multipliers (SetMultipliers >> dispatch)

      cropAmountSettings settings.CropAmount (SetCropAmount >> dispatch)

      div [
        checkboxText "Irrigated" settings.Irrigated (SetIrrigated >> dispatch)
      ]

      mods data modsOpen settings.ModData appDispatch
    ] ]

module LoadSave =
  let [<ReactComponent>] SaveCurrentSettings (props: {| dispatch: _ |}) =
    let name, setName = useState None

    let modalRef = useRef<Browser.Types.HTMLDialogElement option> None

    useLayoutEffect (fun () ->
      match modalRef.current with
      | Some m when not m.``open`` -> m.showModal ()
      | _ -> ())

    fragment [
      button [
        onClick (fun _ -> setName (Some "Untitled Settings"))
        text "Save Current Settings"
      ]

      match name with
      | None -> none
      | Some name ->
        dialog [
          Interop.mkAttr "onClose" (fun _ -> setName None)
          prop.ref modalRef
          children [
            h1 "Save Current Settings As"
            Input.text name (Some >> setName)
            div [
              button [
                onClick (fun _ ->
                  if name <> "" then
                    props.dispatch (SaveSettings name)
                    setName None)
                text "Ok"
              ]
              button [
                onClick (fun _ -> setName None)
                text "Cancel"
              ]
            ]
          ]
        ]
    ]

  let [<ReactComponent>] ImportSave (props: {| dispatch: _ |}) =
    let save, setSave = useState None

    let modalRef = useRef<Browser.Types.HTMLDialogElement option> None

    useLayoutEffect (fun () ->
      match modalRef.current with
      | Some m when not m.``open`` -> m.showModal ()
      | _ -> ())

    fragment [
      label [ Class.fileInput; children [
        ofStr "Import Save"
        input [
          prop.type'.file
          onChange (fun (e: Browser.Types.File) ->
            let text: string JS.Promise = e?text()
            text.``then`` (fun text ->
              let save = Data.loadSaveGame text
              save |> Option.iter (fst >> LoadSaveGame >> props.dispatch)
              setSave (Some save))
            |> ignore
          )
        ]
      ] ]

      match save with
      | None | Some (Some (_, [||])) -> none
      | Some save ->
        dialog [
          Interop.mkAttr "onClose" (fun _ -> setSave None)
          prop.ref modalRef
          children [
            match save with
            | None ->
              h1 "Invalid Save"
              ofStr "Failed to laod the save game. Did you pick the right file?"
            | Some (_, missing) ->
              h1 "Warning"
              ofStr "Failed to load the following data from the save game:"
              ul (missing |> Array.map (ofStr >> li))
            div [
                button [
                onClick (fun _ -> setSave None)
                text "Ok"
              ]
            ]
          ]
        ]
    ]

  let [<ReactComponent>] RenamePreset (props: {| i: int; name: string; dispatch: _ |}) =
    let name, setName = useState None

    let modalRef = useRef<Browser.Types.HTMLDialogElement option> None

    useLayoutEffect (fun () ->
      match modalRef.current with
      | Some m when not m.``open`` -> m.showModal ()
      | _ -> ())

    fragment [
      button [
        onClick (fun _ -> setName (Some props.name))
        text "Edit"
      ]
      match name with
      | None -> none
      | Some name ->
        dialog [
          Interop.mkAttr "onClose" (fun _ -> setName None)
          prop.ref modalRef
          children [
            h1 "Rename"
            Input.text name (Some >> setName)
            div [
              button [
                onClick (fun _ ->
                  if name <> "" then
                    props.dispatch (RenameSettings (props.i, name))
                    setName None)
                text "Ok"
              ]
              button [
                onClick (fun _ -> setName None)
                text "Cancel"
              ]
            ]
          ]
        ]
    ]

  let tab app dispatch =
    let saveDispatch = SetSavedSettings >> dispatch
    let loadDispatch = LoadSettings >> SetState >> dispatch
    div [
      ul (app.SavedSettings |> List.mapi (fun i (name, settings) ->
        li [
          ofStr name
          button [
            onClick (fun _ -> loadDispatch settings)
            text "Load"
          ]
          RenamePreset {| i = i; name = name; dispatch = saveDispatch |}
          button [
            onClick (fun _ -> saveDispatch (DeleteSettings i))
            text "x"
          ]
        ]
      ))

      div [ style [ style.display.flex; style.flexDirection.column; style.width.maxContent ]; children [
        SaveCurrentSettings {| dispatch = saveDispatch |}

        ImportSave {| dispatch = saveDispatch |}

        button [
          onClick (fun _ -> loadDispatch (snd Data.defaultSavedSettings.Value[0]))
          text "Reset Settings to Default"
        ]

        button [
          // add warning
          onClick (fun _ -> dispatch HardReset)
          text "Hard Reset"
        ]
      ] ]
    ]

let section app dispatch =
  let appDispatch = dispatch
  let dispatch = SetState >> dispatch
  let { UI = ui; Settings = settings } = app.State
  section [ prop.id "settings"; children [
    viewTabs SetSettingsTab unitUnionCases<SettingsTab> ui.SettingsTab (SetUI >> dispatch)
    match ui.SettingsTab with
    | Skills -> Skills.tab settings.Game.Skills (SetSkills >> SetGameVariables >> SetSettings >> dispatch)
    | Crops -> Crops.tab app dispatch
    | Fertilizers -> Fertilizers.tab app dispatch
    | Misc -> Misc.tab (ui.OpenDetails.Contains OpenDetails.Mod) app.Data settings.Game dispatch
    | SettingsTab.LoadSettings -> LoadSave.tab app appDispatch
  ] ]
