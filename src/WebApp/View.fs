module StardewValleyStonks.WebApp.View

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update

// limitations: ranker chooses dateSpan with most harvests (least days if tie) if there are two dateSpans

// fix flower crop stage img

// set selectOptions width

// best indicator for Products. Prices

// refactor tables
//   sticky
// refactor model calcs


// Tooltips
// Reduced indicators on growth time, seed price, product price

// crop tooltips;
// growthtime/stages
// lowest seedPrice?
// bestSellPrice?
// Products item
// Seeds price reduction
// lowest fertPrice?


// // Solver view
// // solver solve for max xp

// refactor Graph indicator icons:

// error handling

// Load/Save cleanup

open Fable.Core
open Fable.Core.JsInterop
open Elmish.React
open Feliz

#if DEBUG
importDefault "preact/debug"
#endif

type prop with
  static member inline onChange (handler: nat -> unit) =
    Interop.mkAttr "onChange" (fun (e: Browser.Types.Event) ->
      let value: double = !!e.target?valueAsNumber
      if not <| isNullOrUndefined value
        && not <| System.Double.IsNaN value
        && value >= 0.0
      then
        round value |> unbox<nat> |> handler)

  static member inline onToggle (handler: bool -> unit) =
    Interop.mkAttr "onToggle" (fun (e: Browser.Types.Event) -> handler e.target?``open``)

  static member inline valueOrDefault (n: nat) =
    prop.ref (fun e -> if e |> isNull |> not && !!e?value <> !!n then e?value <- !!n)

  static member inline value (n: nat) = Interop.mkAttr "value" n

open type Html
open type prop
open type React

type type' = prop.type'
let inline id' (id: string) = prop.id id

open Core.Operators
open Core.ExtraTopLevelOperators

let inline min' (n: nat) = Interop.mkAttr "min" n
let inline max' (n: nat) = Interop.mkAttr "max" n

let inline text (str: string) = prop.text str

let inline ofStr (str: string) = Html.text str
let inline ofNat (n: nat) = string n |> ofStr
let inline ofInt (i: int) = Html.text i
let inline ofFloat (x: float) = Html.text x
let inline ofOption elm = elm |> Option.defaultValue none


module Class =
  let checkboxLabel = className "checkbox-label"
  let checkboxImg = className "checkbox-img"
  let iconProcessor = className "icon-processor"
  let iconProcessorLarge = className "icon-processor-large"

  let fileInput = className "file-input"

  let active = className "active"
  let disabled = className "disabled"
  let open' = className "open"
  let tabs = className "tabs"

  let cropQualities = className "crop-qualities"
  let cropQualitiesBars = className "crop-qualities-bars"
  let cropQualitiesProbs = className "crop-qualities-probs"

  let professions = className "professions"
  let skills = className "skills"

  let calendar = className "calendar"
  let calendarSeason = className "calendar-season"
  let calendarHeader = className "calendar-header"
  let calendarDays = className "calendar-days"

  let quality = className "quality"
  let seasonSlot = className "season-slot"
  let date = className "date"

  let breakdownTable = className "breakdown-table"

  let graph = className "graph"
  let graphControls = className "controls"
  let auditGraph = className "audit-graph"
  let auditGraphSelect = className "audit-graph-select"


module Image =
  let path = sprintf "img/%s/%s.png"
  let skillRoot = path "Skills"
  let fertilizerRoot = path "Fertilizers"
  let cropRoot = path "Crops"
  let itemRoot = path "Items"
  let itemPath (item: ItemId) = item |> string |> itemRoot
  let vendorRoot = path "Vendors"
  let processorRoot = path "Processors"
  let uiRoot = path "UI"
  let seasonRoot = path "Seasons"
  let qualityRoot = path "Qualities"
  let productRoot = path "Products"

  let withClass class' path = img [ class'; src path ]

  let at path = img [ src path ]

  let season = Season.name >> seasonRoot >> at
  let item' = itemPath >> at
  let item = Item.id >> item'
  let private withQuality (img: ReactElement) quality =
    div [ Class.quality; children [
      img
      at <| qualityRoot (Quality.name quality)
    ] ]
  let itemQuality' = item' >> withQuality
  let crop = function
    | FarmCrop c -> at (c.Item |> string |> itemRoot)
    | ForageCrop c -> at (c.Seed |> string |> itemRoot)
  let growthStage (i: int) (seed: SeedId) =
    at <| cropRoot $"{seed}/{i}"
  let regrowStage seed =
    at <| cropRoot $"{seed}/Regrow"

  let fertilizer' (fert: string) = fert |> fertilizerRoot |> at //(FertName fertilizer) = fertilizer |> fertilizerRoot |> at
  let fertilizer = Fertilizer.name >> fertilizer'
  let skill = skillRoot >> at
  let profession (profession: Profession) = string profession |> skill
  let vendor (VendorName vendor) = vendor |> vendorRoot |> at
  let processor = function
    | ProcessorName "Mill" -> "Mill" |> processorRoot |> withClass Class.iconProcessorLarge
    | ProcessorName processor -> processor |> processorRoot |> withClass Class.iconProcessor
  let product = function
    | SeedsFromSeedMaker seed -> item' seed
    | Processed product -> item' product.Item
    | product -> at <| productRoot (string product)
  let productQuality = product >> withQuality

  let allQualities = at <| qualityRoot "All"
  let rightArrow = at <| uiRoot "Right Arrow"
  // let rightArrow =
  //   div [
  //     class' "right-arrow"
  //     children (img [ src <| uiRoot "Right Arrow" ])
  //   ]

  module Icon =
    let withClass css path name =
      fragment [
        withClass css path
        ofStr name
      ]

    let at path name =
      fragment [
        at path
        ofStr name
      ]

    let private nameIsPartofPath path name = at (path name) name

    let skill = nameIsPartofPath skillRoot
    let profession (profession: Profession) = string profession |> skill
    let fertilizer = Fertilizer.name >> nameIsPartofPath fertilizerRoot
    let item (item: Item) = at (itemPath item.Id) item.Name
    let item' (data: GameData) = data.Items.Find >> item
    let private withQuality path name quality =
      fragment [
        withQuality (img [ src path ]) quality
        ofStr name
      ]
    let itemQuality item quality = withQuality (itemPath item.Id) item.Name quality
    let itemQuality' (data: GameData) = data.Items.Find >> itemQuality
    let crop (data: GameData) = function
      | FarmCrop c -> at (c.Item |> string |> itemRoot) (FarmCrop.name data.Items.Find c)
      | ForageCrop c -> at (c.Seed |> string |> itemRoot) (ForageCrop.name c)

    let vendor (VendorName name) = nameIsPartofPath vendorRoot name
    let processor = function
      | ProcessorName "Mill" -> withClass Class.iconProcessorLarge (processorRoot "Mill") "Mill"
      | ProcessorName processor -> withClass Class.iconProcessor (processorRoot processor) processor
    let product (data: GameData) item product =
      let name = Product.name data.Items.Find item product
      let path =
        match product with
        | SeedsFromSeedMaker seed -> itemPath seed
        | Processed product -> itemPath product.Item
        | product -> productRoot (string product)
      at path name
    let productQuality (data: GameData) item product quality =
      let name = Product.name data.Items.Find item product
      let path =
        match product with
        | SeedsFromSeedMaker seed -> itemPath seed
        | Processed product -> itemPath product.Item
        | product -> productRoot (string product)
      withQuality path name quality

    let season = Season.name >> nameIsPartofPath seasonRoot


let gold (g: nat) = string g + "g"
let percent value = sprintf "%.0f%%" (value * 100.0)
let percent2 value = sprintf "%.2f%%" (value * 100.0)
let floatFixedRound = sprintf "%.2f"
let floatRound (x: float) = System.Math.Round (x, 2) |> string
let goldFixedRound = sprintf "%.2fg"


let opacityCheckbox children' checked' dispatch =
  label [
    classes [
      "checkbox-label"
      if not checked' then "disabled"
    ]
    children [
      input [
        type'.checkbox
        isChecked checked'
        onCheckedChange dispatch
      ]
      children'
    ]
  ]

let checkboxCustom props children' checked' dispatch =
  label [ yield! props; children [
    input [
      type'.checkbox
      isChecked checked'
      onCheckedChange dispatch
    ]
    Image.withClass Class.checkboxImg (Image.uiRoot <| if checked' then "Checkedbox" else "Checkbox")
    children'
  ] ]

let checkboxWith rest = checkboxCustom (Class.checkboxLabel :: rest)

// let checkboxDisabledWith alsoDisplay disabled msg =
//   checkboxCss (classBase "checkbox-label" "disabled" disabled) alsoDisplay msg
// let checkboxDisabled disabled msg = checkboxDisabledWith [] disabled msg
// let checkboxDisabledText text disabled msg = checkboxDisabledWith [ str text ] disabled msg
let checkbox = checkboxWith [] none
let inline checkboxText str checked' msg = checkboxWith [] (ofStr str) checked' msg

module Select =
  [<ReactComponent(import="default", from="react-select")>]
  let private ReactSelect _ = imported ()

  // workaround for: https://github.com/JedWatson/react-select/issues/5369
  let private wrap value = {| value = value |}
  let private unwrap (o: {| value: _ |}) = o.value

  let select
    (searchable: bool)
    (name: 'a -> string)
    (display: 'a -> ReactElement)
    (options: 'a array)
    (selected: 'a)
    (dispatch: 'a -> unit)
    =
    ReactSelect {|
      className = "rselect"
      options = options |> Array.map wrap
      value = wrap selected
      isSearchable = searchable
      getOptionLabel = unwrap >> name
      formatOptionLabel = unwrap >> display
      onChange = unwrap >> dispatch
    |}

  let inline options name display options selected dispatch = select false name display options selected dispatch

  let inline unitUnion (selected: 'a) dispatch = select false Reflection.getCaseName (Reflection.getCaseName >> ofStr) unitUnionCases<'a> selected dispatch


let labeled label element =
  Html.label [
    ofStr label
    element
  ]


let viewTab toString msg tab currentTab dispatch =
  li [
    if currentTab = tab then Class.active
    children (
      button [
        onClick (fun _ -> dispatch <| msg tab)
        text <| toString tab
      ] )
  ]

let viewTabsWith toString props msg tabs currentTab dispatch =
  ul [ Class.tabs; yield! props; children (tabs |> Array.map (fun tab ->
    lazyView2 (viewTab toString msg tab) currentTab dispatch))
  ]

let viewTabsCss css msg = viewTabsWith (box >> Reflection.getCaseName) css msg
let viewTabs msg = viewTabsWith (box >> Reflection.getCaseName) [] msg


// let warningIcon =
//   img [
//     class' "alert"
//     src (Image.uiRoot "Warning")
//   ]

// let errorIcon =
//   img [
//     class' "alert"
//     src (Image.uiRoot "Error")
//   ]

let debouncer timeout (f : _ -> unit) =
  let mutable last = None
  fun value ->
    last |> Option.iter Browser.Dom.window.clearInterval
    let delayed _ = f value
    last <- Some <| Browser.Dom.window.setTimeout (delayed, timeout)

let debounce timeout =
  let mutable last = None
  fun (action: unit -> unit) ->
    last |> Option.iter Browser.Dom.window.clearInterval
    let delayed _ = action ()
    last <- Some (Browser.Dom.window.setTimeout (delayed, timeout))

let animatedDetails open' (summary': ReactElement) (children': _ seq) dispatch =
  details [
    isOpen open'
    onToggle dispatch
    children [
      summary summary'
      div [
        if open' then Class.open'
        children children'
      ]
    ]
  ]

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
          type'.checkbox
          isChecked selected
          onCheckedChange (curry SetProfession profession >> dispatch)
        ]
        Image.Icon.profession profession
      ]
    ]

  let qualityClass suffix quality = className ((Quality.name quality).ToLower () + "-" + suffix)

  let cropQualities (qualities: _ Qualities) =
    div [ Class.cropQualities; children [
      div [ Class.cropQualitiesBars; children (Quality.all |> Array.map (fun quality ->
        div [
          quality |> qualityClass "bar"
          // using flex-grow instead of style.width prevents divs from overflowing into next line due to animations
          style [ style.custom ("flexGrow", qualities[quality] * 100.0) ]
        ] ))
      ]

      div [ Class.cropQualitiesProbs; children (Quality.all |> Array.map (fun quality ->
        let prob = qualities[quality]
        div [
          quality |> qualityClass "text"
          style [ style.custom ("flexGrow", prob) ]
          if prob > 0.0 then text (percent2 prob)
        ] ))
      ]
    ] ]

  let skill name skill dispatch =
    fragment [
      Html.span (Image.Icon.skill name)

      // let debounce = debouncerWith (fun str -> tryParseNat str |> Option.iter (fun l -> dispatch { data with Level = l } )) 150
      // let onChange = onChange (fun e -> debounce e.Value)
      let props = [
        min' 0u
        max' Skill.maxLevel
        valueOrDefault skill.Level
        onChange (SetLevel >> dispatch)
        //(debouncerWith dispatch 150) ]
      ]
      label [
        ofStr "Level:"
        input (type'.number :: props)
        input (type'.range :: props)
      ]

      label [
        ofStr "Buff:"
        input [
          type'.number
          min' 0u
          valueOrDefault skill.Buff
          onChange (SetBuff >> dispatch)
          // (debouncerWith dispatch 150)
        ]
      ]
    ]

  let farming skills dispatch =
    let farming = skills.Farming
    div [
      skill "Farming" farming (SetFarming >> dispatch)
      div [ Class.professions; children [
        div [
          profession skills Tiller dispatch
        ]
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
        div [ profession skills Gatherer dispatch ]
        div [ profession skills Botanist dispatch ]
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

let colWidths widths =
  colgroup (widths |> Seq.map (fun (width: float) ->
    col [ style [ style.width (length.percent width) ] ] ))

let viewPrice price =
  fragment [
    match price with
    | Some (NonCustom vendor, price) ->
      ofNat price
      Image.vendor vendor
    | Some (Custom (), price) -> ofNat price
    | None -> ofStr "???"
    // if prices.Length = 0 then ofStr "???" else
    // let lowest = prices |> Array.mapReduce min snd

    // let vendors =
    //   prices |> Array.choose (fun (vendor, price) ->
    //     if price = lowest
    //     then Some ()
    //     else None)
    // match vendors with
    // | [| best |] -> best
    // | multiple -> ul (multiple |> Seq.map li)
  ]

type Column<'item> = {
  Header: ReactElement
  Width: float
  Sort: ('item -> 'item -> int) option
}

let sortTableWith attrs columns displayItem setSort sortCols items =
  let columns = Array.ofSeq columns
  let sortData = Array.create columns.Length None
  let numSorts, items =
    sortCols |> List.fold (fun (i, items) (col, asc) ->
      sortData[col] <- Some (i, asc)
      i + 1,
      match columns[col].Sort with
      | Some sort -> items |> Seq.sortDirectionWith asc sort
      | None -> items)
      (0, items)

  let headers = columns |> Seq.mapi (fun i column ->
    th [
      if column.Sort.IsSome then
        onClick (fun e -> setSort (e.shiftKey || e.ctrlKey, (i, sortData[i] |> Option.defaultOrMap true snd)))
      children [
        column.Header
        Html.span [
          match sortData[i] with
          | Some (i, asc) ->
            let num = if numSorts > 1 then string (numSorts - i) else ""
            text $"^{num}"
            if not asc then
              style [
                style.display.inlineBlock
                style.transform.scale(1, -1)
              ]
          | None ->
            if column.Sort.IsSome then
              text "-"
        ]
      ]
    ] )

  table [ yield! attrs; children [
    // colWidths (columns |> Seq.map Column.width)
    thead [ tr headers ]
    tbody (items |> Seq.map displayItem) ] ]

let viewCustom (selection: Selection<_,_>) viewValue key dispatch =
  fragment [
    match selection.Values.TryFind key with
    | Some value ->
      yield checkbox (selection.Selected.Contains key) (curry SetSelected key >> SelectCustom >> dispatch)
      yield viewValue value

      yield button [
        onClick (fun _ -> RemoveCustom key |> dispatch)
        text "x"
      ]
    | None ->
      yield button [
        onClick (fun _ -> AddCustom key |> dispatch)
        text "+"
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
  let seedVendors = sortKeysByHighestCount Data.gameData.SeedPrices

  let processors =
    Data.gameData.Products.Values
    |> Seq.collect Table.keys
    |> Seq.distinct
    |> Seq.sortWith (Option.noneMaxCompareBy Data.gameData.ProcessorUnlockLevel.TryFind)
    |> Array.ofSeq

  let table app cropSort crops dispatch =
    let data = app.Data
    let settings = app.State.Settings
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SelectCrops >> SetSelections >> SetSettings >> dispatch
    [
      sortTableWith [ className "select" ] [
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
          Sort = Some (Option.noneMaxCompareBy (Crop.seed >> Query.lowestSeedPrice data settings))
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
          let price = Query.seedLowestPriceBuyFrom data settings seed
          // let hasSeedSource =
          //   prices.Length > 0
          //   || Model.canUseSeedMakerForOwnSeeds model seed
          //   || (model.UseRawSeeds.Contains seed && model.SeedMode = StockpileSeeds)
          //   || Model.canUseForageSeeds model crop

          tr [
            key (string seed)
            if not <| Game.cropIsInSeason settings.Game crop then Class.disabled
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

    let processorUnlocked = processors |> Array.map (Game.processorUnlocked data settings.Game.Skills)

    let viewItemRow mainCrop seed item =
      let theItem = data.Items[item]
      let products = data.Products[item]
      let selected = settings.Selected.Products[seed, item]
      tr [
        td [
          if not mainCrop then
            ofStr "|__"
          Image.Icon.item' data item
        ]
        td [
          checkbox (settings.Selected.SellRaw.Contains (seed, item)) (curry SetSelected (seed, item) >> SelectSellRaw >> selectDispatch)
          ofNat <| Game.itemPrice settings.Game theItem productQuality
        ]
        yield! processors |> Array.mapi (fun i processor ->
          match products.TryFind processor with
          | Some product ->
            td [
              if not processorUnlocked[i] then Class.disabled
              children [
                checkbox (selected.Contains processor) (curry SetSelected (seed, item) >> curry SelectProducts processor >> selectDispatch)
                if showNormalizedPrices
                then ofFloat <| Game.productProfit data settings.Game item productQuality product
                else ofNat <| Game.productPrice data settings.Game item productQuality product
              ]
            ]
          | None -> td [] )
        td []
        td [
          viewCustom settings.Selected.CustomSellPrices (fun (price, q) ->
            fragment [
              input [
                type'.number
                min' 0u
                valueOrDefault price
                onChange (flip tuple2 q >> curry SetCustom (seed, item) >> SetCustomSellPrice >> selectDispatch)
              ]
              opacityCheckbox Image.allQualities q (tuple2 price >> curry SetCustom (seed, item) >> SetCustomSellPrice >> selectDispatch)
              // input [
              //   type'.checkbox
              //   isChecked q
              //   onCheckedChange (tuple2 price >> curry SetCustom (seed, item) >> SetCustomSellPrice >> dispatch) ]
            ] )
            (seed, item)
            (SetCustomSellPrice >> selectDispatch)
        ]
      ]

    let keyColWidth = 0.4
    let productWidth = 100.0 * (1.0 - keyColWidth) / float (processors.Length + 2)

    [
      labeled "View with quality: " <| Select.options
        Quality.name
        (Quality.name >> ofStr)
        Quality.all
        productQuality
        (SetProductQuality >> uiDispatch)

      checkboxText "Normalize Prices" showNormalizedPrices (SetShowNormalizedProductPrices >> uiDispatch)

      sortTableWith [ className "products" ] [
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
                    Crop.items crop |> Array.map (tuple2 seed))
                  |> Seq.forall settings.Selected.SellRaw.Contains)
                (selectMany (konst true) >> SelectSellRaw >> selectDispatch)
            ofStr "Raw Crop"
          ]
          Width = productWidth
          Sort = Some <| compareByRev (fun crop -> Query.cropBestItemPriceFrom data settings.Game crop productQuality)
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
                    (settings.Selected.Products |> Map.forall (fun (_, item) selected -> selected.Contains processor || not <| data.Products[item].ContainsKey processor))
                    (selectMany (snd >> data.Products.Find >> Table.containsKey processor) >> curry SelectProducts processor >> selectDispatch)
                  Image.Icon.processor processor
                ]
              ]
            Width = productWidth
            Sort = Some <| compareByRev (fun crop -> Query.cropBestProductPriceFrom data settings.Game crop productQuality processor)
          } )
        {
          Header =
            div [
              if not (data.ForageCrops.Values |> Seq.exists (ForageCrop.seedsRecipeUnlocked settings.Game.Skills)) then Class.disabled
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
          Sort = Some <| compareByRev (fun crop -> if Crop.isForage crop then Some <| Game.itemPrice settings.Game data.Items[item' <| Crop.seed crop] Quality.Normal else None)
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
          Sort = Some <| compareByRev (fun crop -> Query.cropBestCustomPrice settings.Selected crop productQuality)
        }
      ]
        (fun crop ->
          let seed = Crop.seed crop
          keyedFragment (int seed, [
            match crop with
            | FarmCrop c ->
              viewItemRow true seed c.Item
              match c.ExtraItem with
              | Some (item, _) -> viewItemRow false seed item
              | None -> none
            | ForageCrop c ->
              tr [
                td (Image.Icon.crop data crop)
                yield! Seq.replicate (processors.Length + 1) (td [])
                td [
                  if not <| ForageCrop.seedsRecipeUnlocked settings.Game.Skills c then Class.disabled
                  children [
                    checkbox (settings.Selected.SellForageSeeds.Contains seed) (curry SetSelected seed >> SelectSellForageSeeds >> selectDispatch)
                    ofNat <| Game.itemPrice settings.Game data.Items[item' seed] Quality.Normal
                  ]
                ]
                td []
              ]
              yield! c.Items |> Array.map (viewItemRow false seed)
          ] ))
        (SetProductSort >> uiDispatch)
        productSort
        crops
    ]

  let seeds (data: GameData) settings seedSort crops dispatch =
    let uiDispatch = SetUI >> dispatch
    let settingsDispatch = SetSettings >> dispatch
    let selectDispatch = SetSelections >> settingsDispatch

    let selectMany filter = curry SetManySelected (crops |> Seq.filter filter |> Seq.map Crop.seed |> set)

    [
      checkboxText "Joja Membership" settings.Game.JojaMembership (SetJojaMembership >> SetGameVariables >> settingsDispatch)
      labeled "Seed Strategy:" <| Select.unitUnion settings.Profit.SeedStrategy (SetSeedStrategy >> SetProfit >> settingsDispatch)

      let keyColWdith = 0.4
      let width = 100.0 * ((1.0 - keyColWdith) / float seedVendors.Length) // div by 0?
      sortTableWith [ className "prices" ] [
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
            Sort = Some <| Option.noneMaxCompareBy (Crop.seed >> Query.seedPriceValueFrom data settings vendor)
          } )
        {
          Header =
            div [
              if not <| Game.processorUnlocked data settings.Game.Skills Processor.seedMaker then Class.disabled
              children [
                checkbox
                  (data.Crops |> Table.forall (fun seed crop ->
                    settings.Selected.UseSeedMaker.Contains seed
                    || not <| GameData.cropCanGetOwnSeedsFromSeedMaker crop data))
                  (selectMany (flip GameData.cropCanGetOwnSeedsFromSeedMaker data) >> SelectUseSeedMaker >> selectDispatch)
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
              (selectMany Crop.makesOwnSeeds >> SelectUseRawSeeds >> selectDispatch)
            ofStr "Raw Seeds"
          ]
          Width = 0
          Sort = None
        }
        {
          Header =
            div [
              if not (data.ForageCrops.Values |> Seq.exists (ForageCrop.seedsRecipeUnlocked settings.Game.Skills)) then Class.disabled
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
                if GameData.cropCanGetOwnSeedsFromSeedMaker crop data then
                  checkbox (settings.Selected.UseSeedMaker.Contains seed) (curry SetSelected seed >> SelectUseSeedMaker >> selectDispatch)
              ]
            ]
            td [
              if Crop.makesOwnSeeds crop then
                checkbox (settings.Selected.UseHarvestedSeeds.Contains seed) (curry SetSelected seed >> SelectUseRawSeeds >> selectDispatch)
            ]
            td (
              match crop with
              | FarmCrop _ -> []
              | ForageCrop c -> [
                if not <| ForageCrop.seedsRecipeUnlocked settings.Game.Skills c then Class.disabled
                children (checkbox (settings.Selected.UseForageSeeds.Contains seed) (curry SetSelected seed >> SelectUseForageSeeds >> selectDispatch))
              ]
            )
            td [
              viewCustom settings.Selected.CustomSeedPrices (fun price ->
                input [
                  type'.number
                  min' 0u
                  valueOrDefault price
                  onChange (curry SetCustom seed >> SetCustomSeedPrice >> selectDispatch)
                ] )
                seed
                (SetCustomSeedPrice >> selectDispatch)
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
      yield! filters.Regrows |> optionFilter (Crop.regrowTime >> Option.isSome)
      yield! filters.Giant |> optionFilter Crop.giant
      yield! filters.Forage |> optionFilter Crop.isForage
    ]
    data.Crops.Values
    |> Seq.filter (fun crop -> filters |> Seq.forall (fun predicate -> predicate crop))
    |> Array.ofSeq

  let private selectFilter name value dispatch =
    Select.options
      (function
        | Some true -> "Yes"
        | Some false -> "No"
        | None -> "Any")
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
  let fertilizerVendors = sortKeysByHighestCount Data.gameData.FertilizerPrices

  let table (data: GameData) settings fertSort open' dispatch =
    let uiDispatch = SetUI >> dispatch
    let selectDispatch = SetSelections >> SetSettings >> dispatch
    animatedDetails
      open'
      (ofStr "Fertilizers")
      [
        checkboxText "Allow No Fertilizer" settings.Selected.NoFertilizer (SelectNoFertilizer >> selectDispatch)
        sortTableWith [ className "select" ] [
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
            Sort = Some <| Option.noneMaxCompareBy (Fertilizer.name >> Query.lowestFertilizerPrice data settings)
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
            let price = Query.fertilizerLowestPriceBuyFrom data settings name

            tr [
              key (string name)
              if price = None then Class.disabled
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

    animatedDetails
      open'
      (ofStr "Prices")
      [
        checkboxText "Pay for Fertilizer" settings.Profit.PayForFertilizer (SetPayForFertilizer >> SetProfit >> dispatch)
        checkboxText "Replace Lost Fertilizer" settings.Profit.ReplaceLostFertilizer (SetReplaceLostFertilizer >> SetProfit >> dispatch)

        let keyColWdith = 0.40
        let width = 100.0 * (1.0 - keyColWdith) / float fertilizerVendors.Length // div by 0?
        sortTableWith [ className "prices" ] [
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
                viewCustom settings.Selected.CustomFertilizerPrices (fun price ->
                  input [
                    type'.number
                    valueOrDefault price
                    onChange (curry SetCustom name >> SetCustomFertilizerPrice >> selectDispatch)
                  ] )
                  name
                  (SetCustomFertilizerPrice >> selectDispatch)
              ]
            ] ] )
          (SetFertilizerPriceSort >> uiDispatch)
          fertPriceSort
          data.Fertilizers.Values
      ]
      (curry SetDetailsOpen OpenDetails.FertilizerPrices >> uiDispatch)

  let tab app dispatch =
    let { UI = ui; Settings = settings } = app.State
    div [ id' "fertilizer-tab"; children [
      table app.Data settings ui.FertilizerSort (ui.OpenDetails.Contains OpenDetails.Fertilizers) dispatch
      prices app.Data settings ui.FertilizerPriceSort (ui.OpenDetails.Contains OpenDetails.FertilizerPrices) dispatch
    ] ]

module Misc =
  let date message (date: Date) dispatch =
    let dispatch = message >> dispatch
    div [ Class.date; children [
      Select.options
        Season.name
        (Season.name >> ofStr)
        Season.all
        date.Season
        (fun season -> dispatch {date with Season = season })
      input [
        type'.number
        min' Date.firstDay
        max' Date.lastDay
        valueOrDefault date.Day
        onChange (fun day -> dispatch { date with Day = day })
      ]
    ] ]

  let multipliers multipliers dispatch =
    div [
      checkboxText "Bear's Knowledge" multipliers.BearsKnowledge (SetBearsKnowledge >> dispatch)
      labeled "Profit Margin:" <| Select.options
        (function
          | 1.0 -> "Normal"
          | margin -> percent margin)
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
        let props = [
          prop.min CropAmount.minGiantCropChecks
          prop.max CropAmount.maxGiantCropChecks
          Interop.mkAttr "step" "any"
          onChange (SetGiantChecksPerTile >> dispatch)
        ]
        input ((valueOrDefault (System.Math.Round (settings.GiantChecksPerTile, 2))) :: type'.number :: props)
        input ((valueOrDefault settings.GiantChecksPerTile) :: type'.range :: props)
      ]
      // giant crop prob
        // P(not Giant) = (1.0 - baseGiantProb) ^ giantCropsPerTile
        // P(Giant) = 1.0 - P(not Giant)
      // avg yield from giant crop / tile
        // (minYield, maxYield) -> avg yield
        // + crops from shaving
        // / 9 tiles

      Select.options
        (Option.defaultOrMap "None" ToolLevel.name)
        (Option.defaultOrMap "None" ToolLevel.name >> ofStr)
        [| None; yield! ToolLevel.all |> Array.map Some |]
        settings.ShavingToolLevel
        (SetShavingToolLevel >> dispatch)
      |> labeled "Shaving Enchantment: "

      checkboxText "Special Charm" settings.SpecialCharm (SetSpecialCharm >> dispatch)
      label [
        ofStr "Luck Buff:"
        input [
          type'.number
          min' 0u
          valueOrDefault settings.LuckBuff
          onChange (SetLuckBuff >> dispatch)
        ]
      ]
      // doubleCrop Chance
    ]

  let mods open' modData dispatch =
    animatedDetails
      open'
      (ofStr "Mods")
      [
        let dispatch = SetModData >> SetGameVariables >> SetSettings >> dispatch
        checkboxText "Quality Products" modData.QualityProducts (SetQualityProducts >> dispatch)
        ul [
          if not modData.QualityProducts then Class.disabled
          children (Crops.processors |> Array.filter ((<>) Processor.seedMaker) |> Array.map (fun processor ->
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
    div [ id' "misc"; children [
      div [ Class.date; children [
        labeled "Location: " <| Select.unitUnion settings.Location (SetLocation >> dispatch)
        // check that not: (startSeason = endSeason and endDay < startday)
        date SetStartDate settings.StartDate dispatch
        date SetEndDate settings.EndDate dispatch
      ] ]

      multipliers settings.Multipliers (SetMultipliers >> dispatch)

      cropAmountSettings settings.CropAmount (SetCropAmount >> dispatch)

      div [
        checkboxText "Irrigated" settings.Irrigated (SetIrrigated >> dispatch)
      ]

      mods modsOpen settings.ModData appDispatch
    ] ]

module LoadSave =
  importDefault "react-pure-modal/dist/react-pure-modal.min.css"

  [<ReactComponent(import="default", from="react-pure-modal")>]
  let private reactModal _ = imported ()

  [<ReactComponent>]
  let saveCurrentSettings (props: {| dispatch: _ |}) =
    let name, setName = useState None

    fragment [
      button [
        onClick (fun _ -> setName (Some "Untitled Settings"))
        text "Save Current Settings"
      ]

      match name with
      | None -> none
      | Some name ->
        reactModal {|
          onClose = (fun () -> setName None; true)
          isOpen = true
          header = ofStr "Save Current Settings As"
          children = [|
            input [
              type'.text
              value name
              onChange (Some >> setName)
            ]
          |]
          footer = [|
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
          |]
        |}
    ]

  [<ReactComponent>]
  let importSave (props: {| dispatch: _ |}) =
    let save, setSave = useState None

    fragment [
      label [ Class.fileInput; children [
        ofStr "Import Save"
        input [
          type'.file
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
      | None | Some (Some (_, [| |])) -> none
      | Some (Some (_, missing)) ->
        reactModal {|
          onClose = (fun () ->
            setSave None
            true
          )
          isOpen = true
          children = missing
        |}
      | Some None ->
        reactModal {|
          onClose = (fun () ->
            setSave None
            true
          )
          isOpen = true
          children = [|
            ofStr "Broken save."
          |]
        |}
    ]

  let tab app dispatch =
    let saveDispatch = SetSavedSettings >> dispatch
    let loadDispatch = LoadSettings >> SetState >> dispatch
    div [
      // | LoadSavedModel i -> { app with Model = snd app.SavedModels[i] }, []
      // | SaveCurrentModel name -> { app with SavedModels = (name, app.Model) :: app.SavedModels }, []
      // | RenameSavedModel (i, name) -> { app with SavedModels = app.SavedModels |> List.updateAt i (name, app.SavedModels |> List.item i |> snd) }, []
      // | DeleteSavedModel i -> { app with SavedModels = app.SavedModels |> List.removeAt i }, []

      ul [
        yield! app.SavedSettings |> List.mapi (fun i (name, settings) ->
          li [
            ofStr name
            button [
              onClick (fun _ -> loadDispatch settings)
              text "Load"
            ]
            button [
              onClick (fun _ -> saveDispatch (DeleteSettings i))
              text "x"
            ]
          ]
        )
      ]

      div [ style [ style.display.flex; style.flexDirection.column; style.width.maxContent ]; children [
        saveCurrentSettings {| dispatch = saveDispatch |}

        importSave {| dispatch = saveDispatch |}

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

let settings app dispatch =
  let appDispatch = dispatch
  let dispatch = SetState >> dispatch
  let { UI = ui; Settings = settings } = app.State
  section [ prop.id "settings"; children [
    viewTabsCss [ Class.tabs ] SetSettingsTab unitUnionCases<SettingsTab> ui.SettingsTab (SetUI >> dispatch)
    match ui.SettingsTab with
    | Skills -> Skills.tab settings.Game.Skills (SetSkills >> SetGameVariables >> SetSettings >> dispatch)
    | Crops -> Crops.tab app dispatch
    | Fertilizers -> Fertilizers.tab app dispatch
    | Misc -> Misc.tab (ui.OpenDetails.Contains OpenDetails.Mod) app.Data settings.Game dispatch
    | SettingsTab.LoadSettings -> LoadSave.tab app appDispatch
  ] ]


open Feliz.Recharts

let selectFromGraph rankItem (pairs: _ array) dispatch i =
  let crop, fert = pairs[i]
  (match rankItem with
  | RankCropsAndFertilizers -> (Some crop, Some fert)
  | RankCrops -> (Some crop, None)
  | RankFertilizers -> (None, Some fert))
  |> Some
  |> SetSelectedCropAndFertilizer
  |> dispatch

let pairImage (data: GameData) (pairs: (SeedId * string option) array) selectPair props =
  let index: int = props?payload?value
  let crop, fert = pairs.[index]
  Svg.svg [
    svg.className "pairSelect"
    svg.onClick (fun _ -> selectPair index)
    svg.x (props?x - 10)
    svg.y (props?y: int)
    svg.width 20
    svg.height 40
    svg.children [
      Svg.image [
        svg.href <| Image.itemRoot (Crop.mainItem data.Crops[crop] |> string)
        svg.width 20
        svg.height 20
      ]
      match fert with
      | Some f ->
        Svg.image [
          svg.href <| Image.fertilizerRoot (string f)
          svg.width 20
          svg.height 20
          svg.y 20
        ]
      | None -> yield! []
    ]
  ]

let chartTooltip (data: GameData) (pairs: (SeedId * string option) array) props =
  // number of harvests
  // item qualities / amounts per harvest
  // products sold + amount + price
  // fertilizer bought, replaced
  // seeds bought, made
  // profit / roi
  // days used / total season days
  // normalized profit /oi
  match props?payload with
  | Some (payload: _ array) when payload.Length > 0 && props?active ->
    let (index: int, result: Result<float, Query.NoProfitReasons>) = payload[0]?payload
    let crop, fert = pairs[index]
    let fertDesc = Option.defaultOrMap "" (fun f -> " with " + string f)
    div [
      div (ofStr (Crop.name data.Items.Find data.Crops[crop] + fertDesc fert))
      match result with
      | Ok profit -> div (ofFloat profit)
      | Error e -> none
    ]
  | _ -> none

let private getPath x y width height =
  $"M {x},{y} h {width} v {height} h -{width} Z"

let barBackground gap selectPair props =
  let x: float = props?x - gap / 2.0
  let y: float = props?y
  let width: float = props?width + gap
  let height: float = props?height
  let i: int = fst props?payload
  Svg.path [
    svg.classes [ "recharts-rectangle"; "pairSelect" ]
    svg.fill "#00000000"
    svg.onClick (fun _ -> selectPair i)
    svg.radius 0
    svg.x x
    svg.y y
    svg.width width
    svg.height height
    svg.d (getPath x y width height)
  ]

let errorBar (pairs: (SeedId * string option) array) props =
  let x: float = props?x
  let y: float = props?y
  let width: float = props?width
  let height: float = props?height
  match snd props?payload with
  | Ok _ ->
    Svg.path [
      svg.className "recharts-rectangle"
      svg.fill "blue"
      svg.radius 0
      svg.x x
      svg.y y
      svg.width width
      svg.height height
      svg.d (getPath x y width height)
    ]
  | Error (flags: Query.NoProfitReasons) ->
    let crop, fert = pairs[fst props?payload]
    let mutable height = 0
    let maxHeight = width * 3.0
    let y: float = props?background?height - maxHeight
    Svg.svg [
      svg.x x
      svg.y y
      svg.width width
      svg.height maxHeight
      svg.children [
        if flags.HasFlag Query.NoProfitReasons.NoFertilizerPrice then
          Svg.image [
            svg.className "pixel"
            svg.href <| Image.fertilizerRoot (string fert)
            svg.height width
          ]
        if flags.HasFlag Query.NoProfitReasons.NotEnoughSeeds then
          Svg.image [
            svg.className "pixel"
            svg.href <| Image.itemRoot (string crop)
            svg.height width
            svg.y width
          ]
        if flags.HasFlag Query.NoProfitReasons.NotEnoughDays then
          Svg.image [
            svg.className "pixel"
            svg.href <| Image.uiRoot "Time"
            svg.width width
            svg.height width
            svg.y (width * 2.0)
          ]
      ]
    ]

let graph ranker model pairs (data: _ array) dispatch =
  let barGap = 4.0
  let selectPair = selectFromGraph ranker.RankItem pairs dispatch
  Recharts.responsiveContainer [
    responsiveContainer.width (length.percent 100)
    responsiveContainer.chart (Recharts.barChart [
      barChart.data data
      barChart.barSize 40
      barChart.barGap barGap
      barChart.children [
        Recharts.yAxis [
          yAxis.unit (RankMetric.unit ranker.RankMetric)
          yAxis.width 60
        ]
        Recharts.tooltip [
          !!Interop.mkAttr "content" (chartTooltip model pairs)
        ]
        Recharts.bar [
          bar.dataKey (snd >> (function Ok y -> y | Error _ -> 0.0))
          bar.fill "blue"
          Interop.mkBarAttr "background" (barBackground barGap selectPair)
          Interop.mkBarAttr "shape" (errorBar pairs)
          Interop.mkBarAttr "onClick" (fun props -> fst props?payload |> selectPair)
        ]
        Recharts.brush [
          brush.startIndex (min (data.Length - 1) (ranker.BrushSpan |> fst |> int) |> max 0)
          brush.endIndex (min (data.Length - 1) (ranker.BrushSpan |> snd |> int) |> max 0)
          brush.height 30
          Interop.mkBrushAttr "onChange" (fun i -> dispatch <| SetBrushSpan (i?startIndex, i?endIndex))
        ]
        Recharts.xAxis [
          xAxis.dataKey (fst: _ -> int)
          xAxis.tick (pairImage model pairs selectPair)
          xAxis.interval 0
          xAxis.height 50
        ]
      ]
    ] )
  ]

let allPairData metric timeNorm data settings =
  let crops =
    Query.selectedInSeasonCrops data settings |> sortByMany [|
      (fun c1 c2 ->
        match Crop.seasons c1, Crop.seasons c2 with
        | Seasons.None, Seasons.None -> 0
        | Seasons.None, _ -> 1
        | _, Seasons.None -> -1
        | s1, s2 -> Seasons.setOrder s1 s2)

      compareBy (Crop.name data.Items.Find)
    |]

  let fertilizers =
    Query.selectedFertilizers data settings
    |> sortByMany [|
      compareBy Fertilizer.speed
      compareBy Fertilizer.quality
      compareBy Fertilizer.name
    |]
    |> Array.map Some
  let fertilizers =
    if settings.Selected.NoFertilizer
    then Array.append [| None |] fertilizers
    else fertilizers

  let metric =
    match metric with
    | RankMetric.Gold -> Query.cropProfit
    | RankMetric.ROI -> Query.cropROI
    | RankMetric.XP -> Query.cropXP

  let data =
    crops |> Array.collect (fun crop ->
      let profit = metric data settings timeNorm crop
      fertilizers |> Array.map (fun fert ->
        (Crop.seed crop, Fertilizer.Opt.name fert), profit fert))

  {|
    Crops = crops |> Array.map Crop.seed
    Fertilizers = fertilizers |> Array.map Fertilizer.Opt.name
    Pairs = data
  |}

let rankBy labelText ranker dispatch =
  fragment [
    ofStr labelText
    Select.options string (fun metric ->
      Html.span [
        text (string metric)
        title (RankMetric.fullName metric)
      ])
      unitUnionCases<RankMetric>
      ranker.RankMetric
      (SetRankMetric >> dispatch)
    Select.unitUnion ranker.TimeNormalization (SetTimeNormalization >> dispatch)
  ]

let graphView ranker (data, settings) dispatch =
  let pairData = allPairData ranker.RankMetric ranker.TimeNormalization data settings
  if pairData.Pairs.Length = 0 then
    div [
      if pairData.Crops.Length = 0 then ofStr "No Crops Selected"
      if pairData.Fertilizers.Length = 0 then ofStr "No Fertilizers Selected"
    ]
  else
    let pairs =
      match ranker.RankItem with
      | RankCropsAndFertilizers -> pairData.Pairs
      | RankCrops -> pairData.Pairs |> Array.groupBy (fst >> fst) |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))
      | RankFertilizers -> pairData.Pairs |> Array.groupBy (fst >> snd) |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))

    let pairData =
      pairs
      |> Array.indexed
      |> Array.map (fun (i, (_, profit)) -> i, profit)

    let pairs = pairs |> Array.map fst

    let setOrder a b =
      let rec next a b =
        if int a = 0 || int b = 0 then
          compare a b
        else
          let c = compare (int b &&& 1) (int a &&& 1)
          if c = 0
          then next (a >>> 1) (b >>> 1)
          else c
      next a b

    pairData |> Array.sortInPlaceWith (fun a b ->
      match snd a, snd b with
      | Ok a, Ok b -> compare b a
      | Ok _, Error _ -> -1
      | Error _, Ok _ -> 1
      | Error a, Error b -> setOrder a b)

    fragment [
      div [ Class.graphControls; children [
        ofStr "Rank"
        Select.options string (fun rankby ->
          Html.span [
            text <| string rankby
            title (
              match rankby with
              | RankCropsAndFertilizers -> "All pairs of crops and fertilizers."
              | RankCrops -> "Pick the best fertilizer for each crop."
              | RankFertilizers -> "Pick the best crop for each fertilizer."
            )
          ])
          unitUnionCases<RankItem>
          ranker.RankItem
          (SetRankItem >> dispatch)

        rankBy "By" ranker dispatch
      ] ]
      div [
        Class.graph
        children [
          lazyView3With
            (fun (data1, _) (data2, _) -> data1 = data2)
            (fun (pairs, pairData) (ranker, data) -> graph ranker data pairs pairData)
            (pairs, pairData)
            (ranker, data)
            dispatch
        ]
      ]
    ]

let growthCalender app seed fertilizer =
  let data = app.Data
  let settings = app.State.Settings
  let crop = data.Crops[seed]
  let fert = fertilizer |> Option.map data.Fertilizers.Find
  match Query.bestGrowthSpan settings.Game fert crop with
  | None -> ofStr (if Game.cropIsInSeason settings.Game crop then "No harvests possible!" else "Crop not in season!")
  | Some span ->
    let firstStageImages =
      span.Stages
      |> Array.mapi (fun i stage ->
        Array.create (int stage) (div [ Image.growthStage i seed ]))
      |> Array.concat
    let last =
      match crop with
      | FarmCrop _ -> Image.growthStage span.Stages.Length seed
      | ForageCrop _ -> Image.item' <| Crop.mainItem crop
    let last = [| div last |]
    let stageImages =
      let first =
        match Crop.regrowTime crop with
        | Some time -> Array.create (int time - 1) (div [ Image.regrowStage seed ])
        | None -> Array.tail firstStageImages
      Array.append first last
    let firstStageImages = Array.append firstStageImages last

    let disabledDay = div [ Class.disabled ]

    div [
      Class.calendar
      children [
        let harvests = span.Harvests
        let span = span.Span
        let unusedDays = span.TotalDays - nat firstStageImages.Length - (harvests - 1u) * nat stageImages.Length
        let days =
          [
            if settings.Game.StartDate.Season = span.StartSeason then
              Array.create (int (settings.Game.StartDate.Day - Date.firstDay)) disabledDay

            Array.create (int unusedDays) (div [])

            firstStageImages

            yield! Seq.replicate (int (harvests - 1u)) stageImages

            if settings.Game.EndDate.Season = span.EndSeason then
              Array.create (int (Date.lastDay - settings.Game.EndDate.Day)) disabledDay
          ]
          |> Array.concat
          |> Array.chunkBySize (int Date.daysInSeason)

        for i, season in Season.span span.StartSeason span.EndSeason |> Seq.indexed do
          let days = days[i]
          div [
            Class.calendarSeason
            children [
              div [
                Class.calendarHeader
                children (Image.Icon.season season)
              ]

              div [
                Class.calendarDays
                children days
              ]
            ]
          ]
      ]
    ]

// Previous Stockpiled Seed                                               1 seed
// or
// Initial Bought Seed from vendor                             -> -gold   1 seed
//                          fertilizer (price) from vendor     -> -gold
// item quality x amount -> product * quality (price) x amount -> gold
//                          item * quality (price) x amount    -> gold
// item quality x amount -> custom * quality (price) x amount  -> gold
//        itemA x amount ->
//        itemB x amount -> Forage Seeds (price) x amount      -> gold  +x seed
//        itemC x amount ->
// item quality x amount -> seedmaker                          ->       +x seed
//                                       item quality x amount ->       +x seed
//         replacement fertilizer (price) from vendor x amount -> -gold
//                   bought seeds (price) from vendor x amount -> -gold +x seed
// Seeds Used                                                           -x seed
//                                                          Total: gold, 1 seed

// (item q) amount -> (item q) price amount : gold seed

let profitBreakdownTable roi timeNorm (data: GameData) settings seed fertName =
  let crop = data.Crops[seed]

  if not <| Game.cropIsInSeason settings.Game crop then ofStr "Crop not in season!" else

  let fert = fertName |> Option.map data.Fertilizers.Find
  match Query.cropProfitData data settings timeNorm crop fert with
  | None -> ofStr "No harvests possible!"
  | Some profitData ->
    let items = Crop.items crop

    let fertilizerBoughtRow replacement amount =
      match profitData.FertilizerPrice with
      | None -> none
      | Some fertCost ->
        tr [
          td [ colSpan 4; children [
            if replacement then ofStr "Replacement "
            Image.Icon.fertilizer <| Option.get fert
            match fertCost with
            | Some (NonCustom vendor, _) ->
              ofStr " from "
              Image.Icon.vendor vendor
            | Some (Custom (), _) ->
              ofStr " (Custom)"
            | None -> ofStr " from ???"
          ] ]
          td [
            match fertCost with
            | Some (_, cost) -> gold cost |> ofStr
            | None -> ofStr "???"
          ]
          td [
            ofStr " x "
            ofStr <| floatFixedRound amount
          ]
          td [
            match fertCost with
            | Some (_, cost) -> ofStr (goldFixedRound <| amount * -float cost)
            | None -> ofStr "???"
          ]
          td []
        ]

    let totalFooter =
      tfoot [
        match profitData.NetProfit with
        | None ->
          tr [
            td [
              colSpan 6
              text "Total"
            ]
            td [
              ofStr "???"
            ]
            td [
              match settings.Profit.SeedStrategy with
              | IgnoreSeeds -> none
              | StockpileSeeds -> ofStr "1.00"
              | BuyFirstSeed -> ofStr "0.00"
            ]
          ]
        | Some profit ->
          tr [
            td [
              colSpan 6
              text "Total"
            ]
            td [
              goldFixedRound profit |> ofStr
            ]
            td [
              match settings.Profit.SeedStrategy with
              | IgnoreSeeds -> none
              | StockpileSeeds -> ofStr "1.00"
              | BuyFirstSeed -> ofStr "0.00"
            ]
          ]
          if not roi && timeNorm <> TotalPeriod then
            tr [
              td [
                colSpan 6
              ]
              td [
                ofStr "/ "
                match timeNorm with
                | PerDay ->
                  profitData.TimeNormalization |> ofFloat
                  ofStr " days"
                | PerSeason ->
                  let value = floatRound profitData.TimeNormalization
                  ofStr value
                  ofStr (if value = "1" then " season" else " seasons")
                | TotalPeriod -> ofInt 1
              ]
              td []
            ]
            tr [
              td [
                colSpan 6
                text ("Total " + string timeNorm)
              ]
              td [
                floatFixedRound (profit / profitData.TimeNormalization) |> ofStr
                match timeNorm with
                | TotalPeriod -> none
                | PerDay -> ofStr "g/day"
                | PerSeason -> ofStr "g/season"
              ]
              td []
            ]
      ]

    div [ Class.breakdownTable; children [
      div [
        div [
          let span = profitData.GrowthSpan.Span
          Image.Icon.season span.StartSeason
          if span.StartSeason <> span.EndSeason then
            Image.rightArrow
            Image.Icon.season span.EndSeason
          ofStr $" ({profitData.GrowthSpan.Harvests} harvests)"
        ]

        table [
          thead [
            tr [
              th [
                colSpan 6
                text "Item"
              ]
              th [ ofStr "Profit" ]
              th [ if settings.Profit.SeedStrategy <> IgnoreSeeds then ofStr "Seeds" ]
            ]
          ]

          tbody [
            if settings.Profit.SeedStrategy = StockpileSeeds then
              tr [
                td [
                  colSpan 6
                  text "Previous Stockpiled Seed"
                ]
                td []
                td [ ofStr "1.00" ]
              ]

            fertilizerBoughtRow false 1.0

            if profitData.FertilizerBought > 1.0 then
              fertilizerBoughtRow true (profitData.FertilizerBought - 1.0)

            if profitData.SeedsBought > 0.0 then
              tr [
                td [ colSpan 4; children [
                  Image.Icon.item' data (seed * 1u<_>)
                  match profitData.SeedPrice with
                  | Some (NonCustom vendor, _) ->
                    ofStr " from "
                    Image.Icon.vendor vendor
                  | Some (Custom (), _) ->
                    ofStr " (Custom)"
                  | None -> ofStr " from ???"
                ] ]
                td [
                  match profitData.SeedPrice with
                  | Some (_, cost) -> gold cost |> ofStr
                  | None -> ofStr "???"
                ]
                td [
                  ofStr " x "
                  ofStr <| floatFixedRound profitData.SeedsBought
                ]
                td [
                  match profitData.SeedPrice with
                  | Some (_, cost) -> ofStr (goldFixedRound <| profitData.SeedsBought * -float cost)
                  | None -> ofStr "???"
                ]
                td [
                  ofStr <| floatFixedRound profitData.SeedsBought
                ]
              ]
          ]

          tbody (profitData.IntoSeedAmounts |> Array.map (fun (item, amounts) ->
            if int item = int seed then
              fragment [
                for i = Quality.highest downto 0 do
                let quality = enum i
                let amount = amounts[quality]
                if amount = 0.0 then none else
                tr [
                  td []
                  td []
                  td []
                  td [
                    Image.Icon.itemQuality' data item quality
                  ]
                  td []
                  td [
                    ofStr " x "
                    ofStr <| floatFixedRound amount
                  ]
                  td []
                  td [
                    ofStr <| floatFixedRound amount
                  ]
                ]
              ]
            elif item = Crop.mainItem crop then
              fragment [
                for i = Quality.highest downto 0 do
                let quality = enum i
                let amount = amounts[quality]
                if amount = 0.0 then none else
                tr [
                  td [
                    Image.Icon.itemQuality' data item quality
                  ]
                  td [
                    ofStr " x "
                    ofStr <| floatFixedRound amount
                  ]
                  td [ Image.rightArrow ]
                  td [
                    Image.Icon.productQuality data item (SeedsFromSeedMaker (seed * 1u<_>)) Quality.Normal
                  ]
                  td []
                  td [
                    ofStr " x "
                    ofStr <| floatFixedRound (amount * Processor.seedMakerAmountWith seed)
                  ]
                  td []
                  td [
                    ofStr <| floatFixedRound (amount * Processor.seedMakerAmountWith seed)
                  ]
                ]
              ]
            else
              assert false
              none))

          if profitData.ForageSeedsSold > 0.0 || profitData.ForageSeedsUsed > 0.0 then
            let totalMade = profitData.ForageSeedsSold + profitData.ForageSeedsUsed
            tbody (items |> Array.mapi (fun i item ->
              tr [
                td [
                  Image.Icon.item' data item
                ]
                td [
                  ofStr " x "
                  ofStr <| floatFixedRound (totalMade / float ForageCrop.forageSeedsPerCraft)
                ]
                td [ Image.rightArrow ]
                if i = 0 then
                  td [ rowSpan items.Length; children [ Image.Icon.item' data (seed * 1u<_>) ] ]
                  td [ rowSpan items.Length; children [
                    ofStr <| gold (Game.seedItemSellPrice data settings.Game seed)
                  ] ]
                  td [ rowSpan items.Length; children [
                    ofStr " x "
                    ofStr <| floatFixedRound totalMade
                  ] ]
                  if profitData.ForageSeedsSold > 0.0 then
                    td [ rowSpan items.Length; children [
                      ofStr <| goldFixedRound (profitData.ForageSeedsSold * float (Game.seedItemSellPrice data settings.Game seed))
                    ] ]
                  if profitData.ForageSeedsUsed > 0.0 then
                    td [ rowSpan items.Length; children [
                      ofStr <| floatFixedRound profitData.ForageSeedsUsed
                    ] ]
                ]
              ))

          tbody [
            if settings.Profit.SeedStrategy <> IgnoreSeeds then
              tr [
                td [
                  colSpan 6
                  text $"{profitData.GrowthSpan.Harvests} Harvests"
                ]
                td []
                td [ ofStr <| floatFixedRound (if Crop.regrows crop then -1.0 else -float profitData.GrowthSpan.Harvests) ]
              ]
          ]

          tbody [
            for i = 0 to items.Length - 1 do
              let item = items[i]
              let soldAmounts = profitData.SoldAmounts[i]
              let sellAs = profitData.SellAs[i]

              for i = Quality.highest downto 0 do
                let quality = enum i
                let amount = soldAmounts[quality]
                if amount = 0.0 then none else
                match sellAs with
                | Some sellAsData ->
                  let product, profit = sellAsData[quality]
                  tr [
                    if product = NonCustom None then
                      td []
                      td []
                      td []
                    else
                      td [
                        Image.Icon.itemQuality' data item quality
                      ]
                      td [
                        ofStr " x "
                        ofStr <| floatFixedRound amount
                      ]
                      td [ Image.rightArrow ]

                    match product with
                    | NonCustom None ->
                      td [
                        Image.Icon.itemQuality' data item quality
                      ]
                      td [
                        ofStr <| gold (nat profit)
                      ]
                      td [
                        ofStr " x "
                        ofStr <| floatFixedRound amount
                      ]
                    | NonCustom (Some product) ->
                      let amountPerItem = Product.amountPerItem product
                      td [
                        Image.Icon.productQuality data item product (Product.outputQuality settings.Game.ModData quality product)
                      ]
                      td [
                        ofStr <| goldFixedRound (profit / amountPerItem)
                      ]
                      td [
                        ofStr " x "
                        ofStr <| floatFixedRound (amount * amountPerItem)
                      ]
                    | Custom (price, preservesQuality) ->
                      td [
                        ofStr "Custom"
                      ]
                      td [
                        ofStr <| gold price
                      ]
                      td [
                        ofStr " x "
                        ofStr <| floatFixedRound amount
                      ]

                    td [
                      ofStr <| goldFixedRound (profit * amount)
                    ]
                    td []
                  ]
                | None ->
                    tr [
                      td [
                        Image.Icon.itemQuality' data item quality
                      ]
                      td [
                        ofStr " x "
                        ofStr <| floatFixedRound amount
                      ]
                      td [ Image.rightArrow ]
                      td [ colSpan 3; text "???" ]
                      td []
                      td []
                    ]
          ]

          totalFooter
        ]
      ]

      if roi then
        let investment = profitData.Investment (settings.Profit.SeedStrategy = BuyFirstSeed)
        let roi = investment |> Option.bind profitData.ROI
        div [
          div [
            ofStr "Investment: "
            ofStr (investment |> Option.defaultOrMap "???" gold)
          ]
          div [
            ofStr "ROI: "
            ofStr (roi |> Option.defaultOrMap "???" (sprintf "%.2f%%"))
          ]
          match roi with
          | Some roi when timeNorm <> TotalPeriod ->
            div [
              ofStr "/ "
              match timeNorm with
              | PerDay ->
                profitData.TimeNormalization |> ofFloat
                ofStr " days"
              | PerSeason ->
                let value = floatRound profitData.TimeNormalization
                ofStr value
                ofStr (if value = "1" then " season" else " seasons")
              | TotalPeriod -> ofInt 1
            ]
            div [
              floatFixedRound (roi / profitData.TimeNormalization) |> ofStr
              match timeNorm with
              | TotalPeriod -> none
              | PerDay -> ofStr "%/day"
              | PerSeason -> ofStr "%/season"
            ]
          | _ -> none
        ]
    ] ]

let xpBreakdownTable timeNorm (data: GameData) settings seed fertName =
  let crop = data.Crops[seed]
  let fert = fertName |> Option.map data.Fertilizers.Find
  match Query.cropXpData data settings timeNorm crop fert with
  | Error e ->
    div [
      if e.HasFlag Query.NoProfitReasons.NotEnoughDays then
        ofStr (if not <| Game.cropIsInSeason settings.Game crop then "Crop not in season!" else "No harvests possible!")
      if e.HasFlag Query.NoProfitReasons.NoFertilizerPrice then
        ofStr "No Fertilizer Price!"
      if e.HasFlag Query.NoProfitReasons.NotEnoughSeeds then
        ofStr "No Seed Source!"
    ]
  | Ok data ->
    div [
      let total = data.xpPerHarvest * float data.Harvests
      div [
        ofStr (sprintf "%.2fxp" data.xpPerHarvest)
        ofStr " x "
        ofStr $"{data.Harvests} harvests"
        ofStr " = "
        ofStr (sprintf "%.2fxp" total)
      ]
      if timeNorm <> TotalPeriod then
        let unit =
          match timeNorm with
          | PerDay -> "day"
          | PerSeason -> "season"
          | TotalPeriod -> ""
        div [
          ofStr (sprintf "%.2fxp" total)
          ofStr " / "
          let value = floatRound data.TimeNormalization
          ofStr value
          ofStr " "
          ofStr (if value = "1" then unit else (unit + "s"))
          ofStr " = "
          ofStr (sprintf "%.2fxp/%s" (total / data.TimeNormalization) unit)
        ]
    ]

// what to show if current crop and/or fertilizer is not selected?
let selectedCropAndFertilizer2 =
  let createFilter: obj -> obj = import "createFilter" "react-select"
  Fable.React.FunctionComponent.Of (fun (props: {| app: _; seed: _; fert: _; dispatch: _ |}) ->
    let app = props.app
    let seed = props.seed
    let fert' = props.fert
    let dispatch = props.dispatch

    let appDispatch = dispatch
    let dispatch = SetRanker >> dispatch
    let data = app.Data
    let { UI = ui; Settings = settings } = app.State
    let ranker = ui.Ranker

    let state = Fable.React.HookBindings.Hooks.useState (ranker.RankMetric, ranker.TimeNormalization)

    let pairData = allPairData (fst state.current) (snd state.current) data settings

    let bestCrop, bestFert =
      if pairData.Pairs.Length = 0 then None, None else
      let bestCrop, bestFert = pairData.Pairs |> Array.maxBy (snd >> Option.ofResult) |> fst

      let bestCrop =
        match fert' with
        | Some fert' ->
          let filtered = pairData.Pairs |> Array.filter (fst >> snd >> (=) fert')
          if filtered.Length = 0 then None else
          filtered
          |> Array.maxBy (snd >> Option.ofResult)
          |> fst
          |> fst
          |> Some
        | None -> Some bestCrop

      let bestFert =
        match seed with
        | Some seed ->
          let filtered = pairData.Pairs |> Array.filter (fst >> fst >> (=) seed)
          if filtered.Length = 0 then None else
          filtered
          |> Array.maxBy (snd >> Option.ofResult)
          |> fst
          |> snd
          |> Some
        | None -> Some bestFert

      bestCrop, bestFert

    let cropDisplayName crop = Crop.name data.Items.Find crop

    let cropOption seed = Choice1Of2 data.Crops[seed]
      // let crop = data.Crops[seed]
      // let name = Crop.name data.Items.Find crop
      // {|
      //   name = name
      //   value = Choice1Of2 crop
      // |}

    let bestCropOption = Choice2Of2 (bestCrop |> Option.map data.Crops.Find)
      // let bestCrop = bestCrop |> Option.map data.Crops.Find
      // let bestName = bestCrop |> Option.defaultOrMap "???" (Crop.name data.Items.Find)
      // {|
      //   name = bestName
      //   value = Choice2Of2 bestCrop
      // |}

    let cropOptions =
      pairData.Crops
      |> Array.map cropOption
      |> Array.append [| bestCropOption |]

    let fertilizerDisplayName fert = fert |> Option.defaultOrMap "No Fertilizer" Fertilizer.name

    let bestFertilizerOption = Choice2Of2 (bestFert |> Option.map (Option.map data.Fertilizers.Find))

    let fertilizerOption fert = Choice1Of2 fert

    let fertilizerOptions =
      pairData.Fertilizers
      |> Array.map (Option.map data.Fertilizers.Find >> fertilizerOption)
      |> Array.append [| bestFertilizerOption |]

    div [ Class.auditGraph; children [
      button [
        onClick (fun _ -> SetSelectedCropAndFertilizer None |> dispatch)
        text "Back"
      ]

      div [ Class.auditGraphSelect; children [
        div [
          Select.select
            true
            (function
              | Choice1Of2 crop -> cropDisplayName crop
              | Choice2Of2 crop -> crop |> Option.defaultOrMap "???" cropDisplayName)
            (fun opt ->
              Html.span [
                match opt with
                | Choice1Of2 crop -> Image.Icon.crop data crop
                | Choice2Of2 bestCrop ->
                  ofStr "Best Crop ("
                  bestCrop |> Option.defaultOrMap none (Image.Icon.crop data)
                  ofStr ")"
              ])
            cropOptions
            (seed |> Option.defaultOrMap bestCropOption (fun seed ->
                cropOptions
                |> Array.tryFind (function
                  | Choice1Of2 crop when Crop.seed crop = seed -> true
                  | _ -> false)
                |> Option.defaultValue (cropOption seed)))
            (fun opt ->
              let seed =
                match opt with
                | Choice1Of2 crop -> Some (Crop.seed crop)
                | Choice2Of2 _ -> None
              dispatch (SetSelectedCropAndFertilizer (Some (seed, fert'))))

          ofStr " with "

          Select.select
            true
            (function
              | Choice1Of2 fert -> fertilizerDisplayName fert
              | Choice2Of2 fert -> fert |> Option.defaultOrMap "???" fertilizerDisplayName)
            (fun opt -> Html.span [
              match opt with
              | Choice1Of2 (Some fert) ->
                Image.Icon.fertilizer fert
              | Choice1Of2 None ->
                ofStr "No Fertilizer"
              | Choice2Of2 bestFertilizer ->
                ofStr "Best Fertilizer ("
                match bestFertilizer with
                | Some (Some fert) -> Image.fertilizer fert
                | _ -> none
                ofStr (bestFertilizer |> Option.defaultOrMap "???" fertilizerDisplayName)
                ofStr ")"
            ])
            fertilizerOptions
            (fert' |> Option.defaultOrMap bestFertilizerOption (fun fert ->
                fertilizerOptions
                |> Array.tryFind (function
                  | Choice1Of2 f when (Option.map Fertilizer.name f) = fert -> true
                  | _ -> false)
                |> Option.defaultValue (fertilizerOption (Option.map data.Fertilizers.Find fert))))
            (fun opt ->
              let fert =
                match opt with
                | Choice1Of2 fert -> Some (Option.map Fertilizer.name fert)
                | Choice2Of2 _ -> None
              SetSelectedCropAndFertilizer (Some (seed, fert)) |> dispatch)
        ]

        div [
          ofStr "Show "
          Select.options string (fun metric ->
            Html.span [
              text (string metric)
              title (RankMetric.fullName metric)
            ])
            unitUnionCases<RankMetric>
            (fst state.current)
            (fun metric -> state.update(fun (_, timeNorm) -> metric, timeNorm))
          Select.unitUnion (snd state.current) (fun timeNorm -> state.update(fun (metric, _) -> metric, timeNorm))
        ]
      ] ]

      match seed |> Option.orElse bestCrop, fert' |> Option.orElse bestFert with
      | Some crop, Some fert ->
        animatedDetails
          (ui.OpenDetails.Contains OpenDetails.RankerProfitBreakdown)
          (ofStr "Profit Breakdown")
          [
            match fst state.current with
            | Gold -> profitBreakdownTable false (snd state.current) data settings crop fert
            | XP -> xpBreakdownTable (snd state.current) data settings crop fert
            | ROI -> profitBreakdownTable true (snd state.current) data settings crop fert
          ]
          (curry SetDetailsOpen OpenDetails.RankerProfitBreakdown >> appDispatch)
        animatedDetails
          (ui.OpenDetails.Contains OpenDetails.RankerGrowthCalendar)
          (ofStr "Growth Calendar")
          [ growthCalender app crop fert ]
          (curry SetDetailsOpen OpenDetails.RankerGrowthCalendar >> appDispatch)
      | Some _, None -> ofStr "Please select a fertilizer."
      | None, Some _ -> ofStr "Please select a crop."
      | None, None -> ofStr "Please select a crop and fertilizer."
    ] ] )


let ranker app dispatch =
  let appDispatch = dispatch
  let dispatch = SetRanker >> dispatch
  let { UI = ui; Settings = settings } = app.State
  let ranker = ui.Ranker
  match ranker.SelectedCropAndFertilizer with
  | Some (crop, fert) -> selectedCropAndFertilizer2 {| app = app; seed = crop; fert = fert; dispatch = appDispatch |}
  | None -> lazyView3 graphView ranker (app.Data, settings) dispatch





let debug app =
  let inline time f x name n =
    Fable.Core.JS.console.time name
    for _ = 1 to n do
      f x |> ignore
    Fable.Core.JS.console.timeEnd name

  ()

let solver app dispatch =
  let data = app.Data
  let settings = app.State.Settings

  let viewIt n (calc: unit -> #seq<YALPS.Solution> * float) =
    div [
      let x, y = calc ()
      ofFloat y
      yield!
        x
        |> Seq.collect (fun res -> res.variables)
        |> Seq.map (fun (n, i) -> div (ofStr $"{n}: {i}"))
      button [
        onClick (fun _ ->
          JS.console.time n
          for _ = 0 to 10 do
            calc() |> ignore
          JS.console.timeEnd n)
        text "Time"
      ]
    ]

  // let calc func () =
  //   func
  //     app.Models.Current
  //     app.CropFertilizerSelections.Current.Crops
  //     (app.CropFertilizerSelections.Current.Fertilizers |> Seq.map (Option.map app.Models.Current.Fertilizers.Find))

  let solve name solver n =
    Fable.Core.JS.console.time name
    let data: Solver.SubRangeSolutionRequest array =
      solver
        data
        settings
        (settings.Selected.Fertilizers |> Seq.map Some |> Seq.append [ if settings.Selected.NoFertilizer then None ] |> Array.ofSeq)
        (settings.Selected.Crops |> Array.ofSeq)
    for _ = 1 to n do
      data |> Solver.solveRanges |> ignore
    Fable.Core.JS.console.timeEnd name

  div [
    button [
      onClick (fun _ -> debug app)
      text "debug"
    ]

    br []

    button [
      onClick (fun _ -> solve "s1" Solver.solutionRequests 100)
      text "s1 x 100"
    ]

    br []

    button [
      onClick (fun _ -> solve "s1" Solver.solutionRequests 1)
      text "s1"
    ]

    let solution, total =
      Solver.solutionRequests
        data
        settings
        (settings.Selected.Fertilizers |> Seq.map Some |> Seq.append [ if settings.Selected.NoFertilizer then None ] |> Array.ofSeq)
        (settings.Selected.Crops |> Array.ofSeq)
      |> Solver.solveRanges

    details [ children [
      summary $"Total: {total}"
      div [
        Class.open'

        children (
          solution
          |> Seq.collect (fun res -> res.variables)
          |> Seq.map (fun (n, i) ->
            let varName =
              n.Split "@"
              |> Array.map (fun s ->
                match System.UInt32.TryParse s with
                | true, value -> data.Crops[value * 1u<_>] |> Crop.name data.Items.Find
                | _ -> s)
              |> String.concat " "

            div (ofStr $"{varName}: {i}"))
        )
      ]
    ] ]
  ]


let viewApp app dispatch =
  let appDispatch = dispatch
  let dispatch = SetState >> dispatch
  let ui = app.State.UI
  let rankerChart = ui.Mode = Ranker && ui.Ranker.SelectedCropAndFertilizer.IsNone
  fragment [
    section [
      id' (if rankerChart then "visualization-graph" else "visualization")
      children [
        viewTabsCss [ Class.tabs ] SetAppMode unitUnionCases<AppMode> ui.Mode (SetUI >> dispatch)
        match ui.Mode with
        | Ranker -> ranker app (SetUI >> dispatch)
        | Solver -> solver app dispatch
      ]
    ]

    settings app appDispatch
  ]


open Elmish
#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

let init () =
  let app = Data.LocalStorage.loadApp ()
  Data.LocalStorage.updateVersion ()
  app, []

let saveState = debouncer 100 Data.LocalStorage.saveState
let saveSettings = debouncer 100 Data.LocalStorage.saveSettings

let update msg app =
  match msg with
  | SetState msg ->
    let state = app.State |> Update.state msg app.Data
    saveState state
    { app with State = state }, []
  | SetSavedSettings msg ->
    let saved = app.SavedSettings |> Update.savedSettings msg app.State.Settings
    saveSettings saved
    { app with SavedSettings = saved }, []
  | SyncSavedSettings saved -> { app with SavedSettings = saved }, []
  | HardReset ->
    Data.LocalStorage.clear ()
    Browser.Dom.window.location.reload ()
    app, []

let view app dispatch =
  try viewApp app dispatch
  with e ->
    div [
      ofStr "Whoops, something went wrong."
      ofStr (sprintf "%A" e)
    ]

Program.mkProgram init update view
|> Program.withReactBatched "app"
|> Program.withSubscription (fun _ -> Cmd.ofSub Data.LocalStorage.subscribe)
|> Program.run
