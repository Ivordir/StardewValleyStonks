module StardewValleyStonks.View

open StardewValleyStonks

// breakdown table, calendar view from graph
// table needs to show roi or xp

// best indicator for Products. Prices

// refactor tables
//   sticky
// refactor model calcs


// // Tooltips
// // Reduced indicators on growth time, seed price, product price

// crop tooltips;
// growthtime/stages
// lowest seedPrice?
// bestSellPrice?
// Products item
// Seeds price reduction
// lowest fertPrice?


// // Solver view
// audit view


// refactor Graph indicator icons:

// error handling

// // Load/Save functionality
// // save game import

// // settings reset / new blank settings

// website indicator for app version, game version

open Fable.Core
open Fable.Core.JsInterop
open Elmish.React
open Feliz

#if DEBUG
importAll "preact/debug"
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
  let graph = className "graph"
  let breakdownTable = className "breakdown-table"
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
    | ForageCrop c -> at (c.Growth.Seed |> string |> itemRoot)
  let growthStage (i: int) (seed: SeedId) =
    at <| cropRoot $"{seed}/{i}"
  let regrowStage seed =
    at <| cropRoot $"{seed}/Regrow"

  let fertilizer' (FertName fertilizer) = fertilizer |> fertilizerRoot |> at
  let fertilizer = Fertilizer.name >> fertilizer'
  let skill = skillRoot >> at
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
    let fertilizer = Fertilizer.nameStr >> nameIsPartofPath fertilizerRoot
    let item (item: Item) = at (itemPath item.Id) item.Name
    let item' model = Model.getItem model >> item
    let private withQuality path name quality =
      fragment [
        withQuality (img [ src path ]) quality
        ofStr name
      ]
    let itemQuality item quality = withQuality (itemPath item.Id) item.Name quality
    let itemQuality' model = Model.getItem model >> itemQuality
    let crop model = function
      | FarmCrop c -> at (c.Item |> string |> itemRoot) (FarmCrop.name (Model.getItem model) c)
      | ForageCrop c -> at (c.Growth.Seed |> string |> itemRoot) (ForageCrop.name c)

    let vendor (VendorName name) = nameIsPartofPath vendorRoot name
    let processor = function
      | ProcessorName "Mill" -> withClass Class.iconProcessorLarge (processorRoot "Mill") "Mill"
      | ProcessorName processor -> withClass Class.iconProcessor (processorRoot processor) processor
    let product model item product =
      let name = Product.name (Model.getItem model) item product
      let path =
        match product with
        | SeedsFromSeedMaker seed -> itemPath seed
        | Processed product -> itemPath product.Item
        | product -> productRoot (string product)
      at path name
    let productQuality model item product quality =
      let name = Product.name (Model.getItem model) item product
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

let inline reactSelect (props: obj) = Fable.React.Helpers.ofImport "default" "react-select" props [||]

let selectUnitUnionWith viewOption allCases (current: 'a) dispatch =
  select [
    valueOrDefault (Reflection.getCaseTag current)
    onChange ((int: string -> _) >> flip Block.item allCases >> dispatch)
    children (allCases |> Block.map' viewOption)
  ]

let selectUnitUnion allCases current dispatch =
  selectUnitUnionWith (fun case ->
    option [
      text <| string case
      valueOrDefault (Reflection.getCaseTag case)
    ] )
    allCases
    current
    dispatch

let selectUnitUnionText text allCases current dispatch =
  label [
    ofStr text
    selectUnitUnion allCases current dispatch
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
  ul [ Class.tabs; yield! props; children (tabs |> Block.map' (fun tab ->
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

let debouncerWith (f : _ -> unit) timeout =
  let mutable last = None
  fun value ->
    last |> Option.iter Browser.Dom.window.clearInterval
    let delayed _ = f value
    last <- Some <| Browser.Dom.window.setTimeout (delayed, timeout)

let debouncer timeout =
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
  let profession profession selected unlocked dispatch =
    label [
      classes [
        if selected then "active"
        if not unlocked then "disabled"
      ]
      children [
        input [
          type'.checkbox
          isChecked selected
          onCheckedChange dispatch
        ]
        Image.Icon.skill profession
      ]
    ]

  let qualityClass suffix quality = className ((Quality.name quality).ToLower () + "-" + suffix)

  let cropQualities (qualities: Qualities) =
    div [ Class.cropQualities; children [
      div [ Class.cropQualitiesBars; children (Quality.all |> Block.map' (fun quality ->
        div [
          quality |> qualityClass "bar"
          // using flex-grow instead of style.width prevents divs from overflowing into next line due to animations
          style [ style.custom ("flexGrow",(qualities[quality] * 100.0)) ]
        ] ))
      ]

      div [ Class.cropQualitiesProbs; children (Quality.all |> Block.map' (fun quality ->
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
        max' 10u
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
      skill "Farming" farming (SetFarmingSkill >> dispatch)
      div [ Class.professions; children [
        div [
          profession "Tiller" farming.Professions.Tiller (Skills.tillerLevelMet skills) (SetTiller >> dispatch)
        ]
        div [
          profession "Artisan" farming.Professions.Artisan (Skills.agriculturistLevelMet skills) (SetArtisan >> dispatch)
          profession "Agriculturist" farming.Professions.Agriculturist (Skills.agriculturistLevelMet skills) (SetAgriculturist >> dispatch)
        ]
      ] ]
      cropQualities (Skills.farmingQualities skills)
    ]

  let foraging skills dispatch =
    let foraging = skills.Foraging
    div [
      skill "Foraging" foraging (SetForagingSkill >> dispatch)
      div [ Class.professions; children [
        div [ profession "Gatherer" foraging.Professions.Gatherer (Skills.gathererLevelMet skills) (SetGatherer >> dispatch) ]
        div [ profession "Botanist" foraging.Professions.Botanist (Skills.botanistLevelMet skills) (SetBotanist >> dispatch) ]
      ] ]
      cropQualities (Skills.foragingQualities skills)
    ]

  let tab skills dispatch =
    div [ Class.skills; children [
      farming skills (SetFarming >> dispatch)
      foraging skills (SetForaging >> dispatch)
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
    | Some (Custom, price) -> ofNat price
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

module Column =
  let inline header column = column.Header
  let inline sort column = column.Sort
  let inline width column = column.Width

let sortTableWith attrs columns displayItem setSort (SortByColumns sortCols) items =
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

// default crop sort
module Crops =
  let table model cropSort crops dispatch =
    let appDispatch = dispatch
    let dispatch = SetModel >> dispatch
    [
      sortTableWith [ className "select" ] [
        {
          Header = checkbox (model.Data.Crops.Keys |> Seq.forall model.SelectedCrops.Contains) (curry SetManySelected (crops |> Seq.map Crop.seed |> set) >> SelectCrops >> dispatch)
          Width = 0
          Sort = None
        }
        {
          Header = ofStr "Crop"
          Width = 40
          Sort = Some (compareBy (Crop.name <| Model.getItem model))
        }
        {
          Header = ofStr "Lowest Seed Price"
          Width = 25
          Sort = Some (sortWithLastBy None (Crop.seed >> Model.lowestSeedPrice model))
        }
        {
          Header = ofStr "Growth Time"
          Width = 10
          Sort = Some (compareBy (Model.cropGrowthTime model None))
        }
        {
          Header = ofStr "Regrow Time"
          Width = 10
          Sort = Some (sortWithLastBy None Crop.regrowTime)
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
          let price = Model.seedLowestPriceBuyFrom model seed
          // let hasSeedSource =
          //   prices.Length > 0
          //   || Model.canUseSeedMakerForOwnSeeds model seed
          //   || (model.UseRawSeeds.Contains seed && model.SeedMode = StockpileSeeds)
          //   || Model.canUseForageSeeds model crop

          tr [
            key (string seed)
            if not <| Model.cropInSeason model crop then Class.disabled
            children [
              td (checkbox (model.SelectedCrops.Contains seed) (curry SetSelected seed >> SelectCrops >> dispatch))
              td (Image.Icon.crop model crop)
              td (viewPrice price)
              td (Model.cropGrowthTime model None crop |> ofNat)
              td (Crop.regrowTime crop |> Option.defaultOrMap none ofNat)
              td (Season.all |> Block.map' (fun season ->
                Html.span [ Class.seasonSlot; children [
                  if Crop.growsInSeason season crop then
                    Image.season season
                ] ]
              ))
            ]
          ] )
        (SetCropSort >> appDispatch)
        cropSort
        crops
    ]

  let products model productSort productQuality showNormalizedPrices crops dispatch =
    let appDispatch = dispatch
    let dispatch = SetModel >> dispatch

    let keys =
      crops
      |> Seq.collect (fun crop ->
        let seed = Crop.seed crop
        Crop.items crop |> Block.map' (fun item -> seed, item))
      |> set

    let selectMany filter = keys |> Seq.filter filter |> set |> curry SetManySelected

    let processorUnlocked = model.Data.Processors |> Block.map' (Model.processorUnlocked model)

    let viewItemRow mainCrop seed item =
      let products = model.Data.Products[item]
      let selected = model.SelectedProducts[seed, item]
      tr [
        td [
          if not mainCrop then
            ofStr "|__"
          Image.Icon.item' model item
        ]
        td [
          checkbox (model.SellRawItems.Contains (seed, item)) (curry SetSelected (seed, item) >> SelectSellRawItems >> dispatch)
          ofNat <| Model.itemPrice model (Model.getItem model item) productQuality
        ]
        yield! model.Data.Processors |> Block.mapi' (fun i processor ->
          match products.TryFind processor with
          | Some product ->
            td [
              if not processorUnlocked[i] then Class.disabled
              children [
                checkbox (selected.Contains processor) (curry SetSelected (seed, item) >> curry SelectProducts processor >> dispatch)
                if showNormalizedPrices
                then ofFloat <| Model.productProfit model (Model.getItem model item) productQuality product
                else ofNat <| Model.productPrice model (Model.getItem model item) productQuality product
              ]
            ]
          | None -> td [] )
        td []
        td [
          viewCustom model.CustomSellPrices (fun (price, q) ->
            fragment [
              input [
                type'.number
                min' 0u
                valueOrDefault price
                onChange (flip tuple2 q >> curry SetCustom (seed, item) >> SetCustomSellPrice >> dispatch)
              ]
              opacityCheckbox Image.allQualities q (tuple2 price >> curry SetCustom (seed, item) >> SetCustomSellPrice >> dispatch)
              // input [
              //   type'.checkbox
              //   isChecked q
              //   onCheckedChange (tuple2 price >> curry SetCustom (seed, item) >> SetCustomSellPrice >> dispatch) ]
            ] )
            (seed, item)
            (SetCustomSellPrice >> dispatch)
        ]
      ]

    let keyColWidth = 0.4
    let productWidth = 100.0 * (1.0 - keyColWidth) / float (model.Data.Processors.Length + 2)

    [
      label [
        ofStr "View with quality: "
        select [
          valueOrDefault (Quality.name productQuality)
          onChange ((Quality.Parse: string -> _) >> SetProductQuality >> appDispatch)
          children (Quality.all |> Block.map' (fun quality ->
            option [
              text <| Quality.name quality
              valueOrDefault (Quality.name quality)
            ]
          ))
        ]
      ]

      checkboxText "Normalize Prices" showNormalizedPrices (SetShowNormalizedProductPrices >> appDispatch)

      sortTableWith [ className "products" ] [
        {
          Header = ofStr "Crop"
          Width = 100.0 * keyColWidth
          Sort = Some <| compareBy (Crop.name <| Model.getItem model)
        }
        {
          Header = fragment [
            checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (model.Data.Crops
                  |> Table.toSeq
                  |> Seq.collect (fun (seed, crop) ->
                    Crop.items crop |> Block.map' (tuple2 seed))
                  |> Seq.forall model.SellRawItems.Contains)
                (selectMany (konst true) >> SelectSellRawItems >> dispatch)
            ofStr "Raw Crop"
          ]
          Width = productWidth
          Sort = Some <| compareByRev (fun crop -> Model.cropBestItemPriceFrom model crop productQuality)
        }
        yield! model.Data.Processors |> Block.mapi' (fun i processor ->
          {
            Header =
              div [
                if not processorUnlocked[i] then Class.disabled
                children [
                  checkboxWith
                    [ onClick (fun e -> e.stopPropagation ()) ]
                    none
                    (model.SelectedProducts |> Map.forall (fun (_, item) selected -> selected.Contains processor || not <| model.Data.Products[item].ContainsKey processor))
                    (selectMany (snd >> model.Data.Products.Find >> Table.containsKey processor) >> curry SelectProducts processor >> dispatch)
                  Image.Icon.processor processor
                ]
              ]
            Width = productWidth
            Sort = Some <| sortWithLastByRev None (fun crop -> Model.cropBestProductPriceFrom model crop productQuality processor)
          } )
        {
          Header =
            div [
              if not (Model.forageCrops model |> Seq.exists (ForageCrop.seedsRecipeUnlocked model.Skills)) then Class.disabled
              children [
                checkboxWith
                  [ onClick (fun e -> e.stopPropagation ()) ]
                  none
                  (Model.forageCropIds model |> Seq.forall model.SellForageSeeds.Contains)
                  (curry SetManySelected (crops |> Seq.filter Crop.isForage |> Seq.map Crop.seed |> set) >> SelectSellForageSeeds >> dispatch)
                ofStr "Forage Seeds"
              ]
            ]
          Width = productWidth
          Sort = Some <| sortWithLastBy None (fun crop -> if Crop.isForage crop then Some <| Model.itemPrice model (Model.getSeedItem model <| Crop.seed crop) Quality.Normal else None)
        }
        { Header = fragment [
            if model.CustomSellPrices.Values.IsEmpty then
              none
            else
              checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (Selection.allSelected model.CustomSellPrices)
                (selectMany model.CustomSellPrices.Values.ContainsKey >> SelectCustom >> SetCustomSellPrice >> dispatch)
            ofStr "Custom"
          ]
          Width = 0
          Sort = Some <| sortWithLastBy None (fun crop -> Model.cropBestCustomPrice model crop productQuality)
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
                td (Image.Icon.crop model crop)
                yield! Seq.replicate (model.Data.Processors.Length + 1) (td [])
                td [
                  if not <| ForageCrop.seedsRecipeUnlocked model.Skills c then Class.disabled
                  children [
                    checkbox (model.SellForageSeeds.Contains seed) (curry SetSelected seed >> SelectSellForageSeeds >> dispatch)
                    ofNat <| Model.itemPrice model (Model.getSeedItem model seed) Quality.Normal
                  ]
                ]
                td []
              ]
              yield! c.Items |> Block.map' (viewItemRow false seed)
          ] ))
        (SetProductSort >> appDispatch)
        productSort
        crops
    ]

  let seeds model seedSort crops dispatch =
    let appDispatch = dispatch
    let dispatch = SetModel >> dispatch

    let selectMany filter = curry SetManySelected (crops |> Seq.filter filter |> Seq.map Crop.seed |> set)

    [
      summary [ text "Seeds" ]

      checkboxText "Joja Membership" model.JojaMembership (SetJojaMembership >> dispatch)

      selectUnitUnionText "Seeds:" SeedMode.all model.SeedMode (SetSeedMode >> dispatch)

      let keyColWdith = 0.4
      let width = 100.0 * ((1.0 - keyColWdith) / float model.Data.SeedVendors.Length)
      sortTableWith [ className "prices" ] [
        {
          Header = ofStr "Crop"
          Width = 100.0 * keyColWdith
          Sort = Some <| compareBy (Crop.name <| Model.getItem model)
        }
        yield! model.Data.SeedVendors |> Block.map' (fun vendor ->
          {
            Header = fragment [
              checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (model.SelectedSeedPrices |> Map.forall (fun key selected -> selected |> Set.contains vendor || not (model.Data.SeedPrices[key].ContainsKey vendor)))
                (selectMany (Crop.seed >> model.Data.SeedPrices.Find >> Table.containsKey vendor) >> curry SelectSeedPrices vendor >> dispatch)
              Image.Icon.vendor vendor
            ]
            Width = width
            Sort = Some <| sortWithLastBy None (Crop.seed >> Model.seedPriceValueFrom model vendor)
          } )
        {
          Header =
            div [
              if not <| Model.processorUnlocked model Processor.seedMaker then Class.disabled
              children [
                checkbox
                  (model.Data.Crops |> Table.forall (fun seed crop ->
                    model.UseSeedMaker.Contains seed
                    || not <| GameData.canGetOwnSeedsFromSeedMaker crop model.Data))
                  (selectMany (flip GameData.canGetOwnSeedsFromSeedMaker model.Data) >> SelectUseSeedMaker >> dispatch)
                Image.Icon.processor Processor.seedMaker
              ]
            ]
          Width = 0
          Sort = None
        }
        {
          Header = fragment [
            checkbox
              (model.Data.Crops |> Table.forall (fun seed crop -> not <| Crop.makesOwnSeeds crop || model.UseRawSeeds.Contains seed))
              (selectMany Crop.makesOwnSeeds >> SelectUseRawSeeds >> dispatch)
            ofStr "Raw Seeds"
          ]
          Width = 0
          Sort = None
        }
        {
          Header =
            div [
              if not (Model.forageCrops model |> Seq.exists (ForageCrop.seedsRecipeUnlocked model.Skills)) then Class.disabled
              children [
                checkbox
                  (model.Data.Crops.Values
                    |> Seq.filter Crop.isForage
                    |> Seq.map Crop.seed
                    |> Seq.forall model.UseForageSeeds.Contains)
                  (selectMany Crop.isForage >> SelectUseForageSeeds >> dispatch)
                ofStr "Forage Seeds"
              ]
            ]
          Width = 0
          Sort = None
        }
        {
          Header = fragment [
            if model.CustomSeedPrices.Values.IsEmpty then
              none
            else
              checkboxWith
                [ onClick (fun e -> e.stopPropagation ()) ]
                none
                (Selection.allSelected model.CustomSeedPrices)
                (selectMany (Crop.seed >> model.CustomSeedPrices.Values.ContainsKey) >> SelectCustom >> SetCustomSeedPrice >> dispatch)
            ofStr "Custom"
          ]
          Width = 0
          Sort = Some <| sortWithLastBy None (Crop.seed >> model.CustomSeedPrices.Values.TryFind)
        }
      ]
        (fun crop ->
          let seed = Crop.seed crop
          tr [ key (string seed); children [
            td (Image.Icon.crop model crop)
            yield! model.Data.SeedVendors |> Block.map' (fun vendor ->
              td [
                match model.Data.SeedPrices[seed].TryFind vendor with
                | Some price ->
                  yield checkbox (model.SelectedSeedPrices[seed].Contains vendor) (curry SetSelected seed >> curry SelectSeedPrices vendor >> dispatch)
                  yield Model.seedPrice model seed price |> ofNat
                | None -> yield none
              ] )
            td [
              if not <| Model.processorUnlocked model Processor.seedMaker then Class.disabled
              children [
                if GameData.canGetOwnSeedsFromSeedMaker crop model.Data then
                  checkbox (model.UseSeedMaker.Contains seed) (curry SetSelected seed >> SelectUseSeedMaker >> dispatch)
              ]
            ]
            td [
              if Crop.makesOwnSeeds crop then
                checkbox (model.UseRawSeeds.Contains seed) (curry SetSelected seed >> SelectUseRawSeeds >> dispatch)
            ]
            td (
              match crop with
              | FarmCrop _ -> []
              | ForageCrop c -> [
                if not <| ForageCrop.seedsRecipeUnlocked model.Skills c then Class.disabled
                children (checkbox (model.UseForageSeeds.Contains seed) (curry SetSelected seed >> SelectUseForageSeeds >> dispatch))
              ]
            )
            td [
              viewCustom model.CustomSeedPrices (fun price ->
                input [
                  type'.number
                  min' 0u
                  valueOrDefault price
                  onChange (curry SetCustom seed >> SetCustomSeedPrice >> dispatch)
                ] )
                seed
                (SetCustomSeedPrice >> dispatch)
            ]
          ] ] )
        (SetSeedSort >> appDispatch)
        seedSort
        crops
    ]

  let filteredCrops app =
    let filters = app.CropFilters
    let model = app.Model
    let optionFilter projection filterValue = filterValue |> Option.map (fun value -> projection >> (=) value) |> Option.toList
    let filters = [
      if filters.InSeason then Model.cropInSeason model else Crop.growsInSeasons filters.Seasons
      yield! filters.Regrows |> optionFilter (Crop.regrowTime >> Option.isSome)
      yield! filters.Giant |> optionFilter (function FarmCrop c -> c.Amount.Giant | ForageCrop _ -> false)
      yield! filters.Forage |> optionFilter Crop.isForage
    ]
    model.Data.Crops.Values
    |> Seq.filter (fun crop -> filters |> Seq.forall (fun predicate -> predicate crop))
    |> Array.ofSeq

  let private selectFilter name value dispatch =
    label [
      ofStr name
      select [
        valueOrDefault
          (match value with
          | Some true -> "Yes"
          | Some false -> "No"
          | None -> "Any")
        onChange ((function
          | "Yes" -> Some true
          | "No" -> Some false
          | _ -> None) >> dispatch)
        children [
          option [ valueOrDefault "Any"; text "Any" ]
          option [ valueOrDefault "Yes"; text "Yes" ]
          option [ valueOrDefault "No"; text "No" ]
        ]
      ]
    ]

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
          children (Season.all |> Block.map' (fun season ->
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
    let crops = filteredCrops app
    div [
      cropFilter app.CropFilters (SetCropFilters >> dispatch)

      animatedDetails
        (app.OpenDetails.Contains OpenDetails.Crops)
        (ofStr "Crops")
        (table app.Model app.CropSort crops dispatch)
        (curry SetDetailsOpen OpenDetails.Crops >> dispatch)

      animatedDetails
        (app.OpenDetails.Contains OpenDetails.Products)
        (ofStr "Products")
        (products app.Model app.ProductSort app.ProductQuality app.ShowNormalizedProductPrices crops dispatch)
        (curry SetDetailsOpen OpenDetails.Products >> dispatch)

      animatedDetails
        (app.OpenDetails.Contains OpenDetails.SeedSources)
        (ofStr "Seeds")
        (seeds app.Model app.SeedSort crops dispatch)
        (curry SetDetailsOpen OpenDetails.SeedSources >> dispatch)
    ]


module Fertilizers =
  let table model fertSort open' dispatch =
    let appDispatch = dispatch
    let dispatch = SetModel >> dispatch
    animatedDetails
      open'
      (ofStr "Fertilizers")
      [
        checkboxText "Allow No Fertilizer" model.AllowNoFertilizer (SetAllowNoFertilizer >> dispatch)
        sortTableWith [ className "select" ] [
          {
            Header =
              checkbox
                (model.Data.Fertilizers.Keys |> Seq.forall model.SelectedFertilizers.Contains)
                (curry SetManySelected (set model.Data.Fertilizers.Keys) >> SelectFertilizers >> dispatch)
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
            Sort = Some <| sortWithLastBy None (Fertilizer.name >> Model.lowestFertilizerPrice model)
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
            let price = Model.fertilizerLowestPriceBuyFrom model name

            tr [
              key (string name)
              if price = None then Class.disabled
              children [
                td (checkbox (model.SelectedFertilizers.Contains name) (curry SetSelected name >> SelectFertilizers >> dispatch))
                td (Image.Icon.fertilizer fertilizer)
                td (viewPrice price)
                td (fertilizer.Speed |> percent |> ofStr)
                td (Skills.farmingQualitiesFrom fertilizer model.Skills |> Skills.cropQualities)
              ]
            ] )
          (SetFertilizerSort >> appDispatch)
          fertSort
          model.Data.Fertilizers.Values
      ]
      (curry SetDetailsOpen OpenDetails.Fertilizers >> appDispatch)

  let prices model fertPriceSort open' dispatch =
    let appDispatch = dispatch
    let dispatch = SetModel >> dispatch

    let selectMany filter = curry SetManySelected (model.Data.Fertilizers.Keys |> Seq.filter filter |> set)

    animatedDetails
      open'
      (ofStr "Prices")
      [
        checkboxText "Pay for Fertilizer" model.PayForFertilizer (SetPayForFertilizer >> dispatch)
        checkboxText "Replace Lost Fertilizer" model.ReplaceLostFertilizer (SetReplaceLostFertilizer >> dispatch)

        let keyColWdith = 0.40
        let width = 100.0 * (1.0 - keyColWdith) / float model.Data.FertilizerVendors.Length
        sortTableWith [ className "prices" ] [
          {
            Header = ofStr "Fertilizer"
            Width = 100.0 * keyColWdith
            Sort = Some <| compareBy Fertilizer.name
          }
          yield! model.Data.FertilizerVendors |> Block.map' (fun vendor ->
            {
              Header = fragment [
                checkboxWith
                  [ onClick (fun e -> e.stopPropagation ()) ]
                  none
                  (model.SelectedFertilizerPrices |> Map.forall (fun key selected -> selected |> Set.contains vendor || not (model.Data.FertilizerPrices[key].ContainsKey vendor)))
                  (selectMany (model.Data.FertilizerPrices.Find >> Table.containsKey vendor) >> curry SelectFertilizerPrices vendor >> dispatch)
                Image.Icon.vendor vendor
              ]
              Width = width
              Sort = Some <| sortWithLastBy None (Fertilizer.name >> model.Data.FertilizerPrices.Find >> Table.tryFind vendor)
            } )
          {
            Header = fragment [
              if model.CustomFertilizerPrices.Values.IsEmpty then
                none
              else
                checkboxWith
                  [ onClick (fun e -> e.stopPropagation ()) ]
                  none
                  (Selection.allSelected model.CustomFertilizerPrices)
                  (selectMany model.CustomFertilizerPrices.Values.ContainsKey >> SelectCustom >> SetCustomFertilizerPrice >> dispatch)
              ofStr "Custom"
            ]
            Width = 0
            Sort = Some <| sortWithLastBy None (Fertilizer.name >> model.CustomFertilizerPrices.Values.TryFind)
          }
        ]
          (fun fert ->
            let name = fert.Name
            tr [ key (string name); children [
              td (Image.Icon.fertilizer fert)
              yield! model.Data.FertilizerVendors |> Block.map' (fun vendor ->
                td [
                  match model.Data.FertilizerPrices[name].TryFind vendor with
                  | Some price ->
                    yield checkbox (model.SelectedFertilizerPrices[name].Contains vendor) (curry SetSelected name >> curry SelectFertilizerPrices vendor >> dispatch)
                    yield ofNat price
                  | None -> yield none
                ] )
              td [
                viewCustom model.CustomFertilizerPrices (fun price ->
                  input [
                    type'.number
                    valueOrDefault price
                    onChange (curry SetCustom name >> SetCustomFertilizerPrice >> dispatch)
                  ] )
                  name
                  (SetCustomFertilizerPrice >> dispatch)
              ]
            ] ] )
          (SetFertilizerPriceSort >> appDispatch)
          fertPriceSort
          model.Data.Fertilizers.Values
      ]
      (curry SetDetailsOpen OpenDetails.FertilizerPrices >> appDispatch)

  let tab app dispatch =
    div [ id' "fertilizer-tab"; children [
      table app.Model app.FertilizerSort (app.OpenDetails.Contains OpenDetails.Fertilizers) dispatch
      prices app.Model app.FertilizerPriceSort (app.OpenDetails.Contains OpenDetails.FertilizerPrices) dispatch
    ] ]

module Misc =
  let date message (Date(season, day)) dispatch =
    let dispatch = message >> dispatch
    div [ Class.date; children [
      select [
        valueOrDefault (Season.name season)
        onChange ((Seasons.Parse: string -> _) >> Season.ofSeasons >> flip (curry Date) day >> dispatch)
        children (Season.all |> Block.map' (fun season ->
          option [
            text <| Season.name season
            valueOrDefault (Season.name season)
          ] ))
      ]
      input [
        type'.number
        min' Date.firstDay
        max' Date.lastDay
        valueOrDefault day
        onChange (fun day -> Date(season, day) |> dispatch)
      ]
    ] ]

  let multipliers model multipliers dispatch =
    div [
      checkboxText "Bear's Knowledge" multipliers.BearsKnowledge (SetBearsKnowledge >> dispatch)
      label [
        ofStr "Profit Margin:"
        select [
          valueOrDefault multipliers.ProfitMargin
          onChange ((float: string -> _) >> SetProfitMargin >> dispatch)
          children ({ 1.0..(-0.25)..0.25 } |> Seq.map (fun multiplier ->
            option [
              text (if multiplier = 1.0 then "Normal" else percent multiplier)
              valueOrDefault multiplier
            ] ))
        ]
      ]
      div [
        ofStr "Apply Tiller to Foraged Fruit:"
        checkboxWith [] (Image.Icon.item' model Item.blackberry) (multipliers.ForagedFruitTillerOverrides.Contains Item.blackberry) (curry SetForagedFruitTillerOverrides Item.blackberry >> dispatch)
        let grape = 398<ItemNum>
        checkboxWith [] (Image.Icon.item' model grape) (multipliers.ForagedFruitTillerOverrides.Contains grape) (curry SetForagedFruitTillerOverrides grape >> dispatch)
      ]
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
      div [
        ofStr "Shaving Enchantment"
        select [
          valueOrDefault (settings.ShavingToolLevel |> Option.defaultOrMap -1 int)
          onChange ((int: string -> _ ) >> (function | -1 -> None | i -> Some <| nat i) >> SetShavingToolLevel >> dispatch)
          children [
            option [ valueOrDefault -1; text "None" ]
            option [ valueOrDefault 0; text "Normal" ]
            option [ valueOrDefault 1; text "Copper" ]
            option [ valueOrDefault 2; text "Steel" ]
            option [ valueOrDefault 3; text "Gold" ]
            option [ valueOrDefault 4; text "Iridium" ]
          ]
        ]
      ]
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

  let mods open' processors modData dispatch =
    animatedDetails
      open'
      (ofStr "Mods")
      [
        let dispatch = SetModData >> SetModel >> dispatch
        checkboxText "Quality Products" modData.QualityProducts (SetQualityProducts >> dispatch)
        ul [
          if not modData.QualityProducts then Class.disabled
          children (processors |> Block.filter ((<>) Processor.seedMaker) |> Block.map' (fun processor ->
            li [
              checkboxWith []
                (Image.Icon.processor processor)
                (modData.QualityProcessors |> Set.contains processor)
                (curry SetQualityProcessors processor >> dispatch)
            ]
          ))
        ]
      ]
      (curry SetDetailsOpen OpenDetails.Mod >> dispatch)

  let tab modsOpen model dispatch =
    let appDispatch = dispatch
    let dispatch = SetModel >> dispatch
    div [ id' "misc"; children [
      div [ Class.date; children [
        selectUnitUnionText "Location:" Location.all model.Location (SetLocation >> dispatch)
        // check that not: (startSeason = endSeason and endDay < startday)
        date SetStartDate model.StartDate dispatch
        date SetEndDate model.EndDate dispatch
      ] ]

      multipliers model model.Multipliers (SetMultipliers >> dispatch)

      cropAmountSettings model.CropAmount (SetCropAmount >> dispatch)

      div [
        checkboxText "Irrigated" model.Irrigated (SetIrrigated >> dispatch)
      ]

      mods modsOpen model.Data.Processors model.ModData appDispatch
    ] ]

let settings app dispatch =
  section [ prop.id "settings"; children [
    viewTabsCss [ Class.tabs ] SetSettingsTab SettingsTab.all app.SettingsTab dispatch
    match app.SettingsTab with
    | Skills -> Skills.tab app.Model.Skills (SetSkills >> SetModel >> dispatch)
    | Crops -> Crops.tab app dispatch
    | Fertilizers -> Fertilizers.tab app dispatch
    | Misc -> Misc.tab (app.OpenDetails.Contains OpenDetails.Mod) app.Model dispatch
    | LoadSettings -> none
  ] ]


// module Re = Fable.Recharts
// let graphView (model: Model) (crops, fertilizers) dispatch =
//   // div []
//   //   [ checkboxText "Show Unprofitable Combinations" ToggleShowUnprofitable model.ShowUnprofitable dispatch
//   //     selectFertilizerOption (Model.activeFertilizers model) [ str "Select a Fertilizer" ] SetCompareFertilizer model.CompareFertilizer dispatch
//   //     selectStringOptionWith "Any" (Model.activeCrops model) [ str "Select a Crop" ] SetCompareCrop model.CompareCrop dispatch ]

//   //what if there are no valid/profitable pairs -> display?
//   let combos =
//     let crops = Seq.map model.Entities.Crops.Find crops |> Seq.filter (Crop.growsInSeasons (Date.seasonsBetween model.StartDate model.EndDate)) |> Array.ofSeq
//     Array.sortInPlaceBy (Crop.name model.Entities.Items.Find) crops
//     Array.sortInPlaceWith (sortWithLastBy Seasons.None Crop.seasons) crops
//     fertilizers
//     |> Seq.map (Option.map model.Entities.Fertilizers.Find)
//     |> Array.ofSeq
//     |> Array.allPairs crops


//   let pairs =
//     combos |> Array.mapi (fun i (crop, fert) ->
//       let profit = Model.totalNetProfit model crop fert (Model.consecutiveDays model <| Crop.seasons crop)
//       i, profit)
//   pairs |> Array.sortInPlaceWith (fun a b -> -compare (snd a) (snd b))

//   Re.responsiveContainer []
//     [ Re.barChart
//         [ Re.Props.Chart.Data pairs ]
//         [ Re.yaxis [] []
//           // Re.tooltip
//           //   [ Re.Props.Tooltip.Content (chartTooltip comboDict.TryFind) ]
//           //   []
//           Re.bar
//             [ Re.Props.Cartesian.DataKey snd
//               Re.Props.Cartesian.BarSize 40.0
//               Re.Props.Cell.Fill "blue"
//               // Re.Props.Cartesian.OnClick (fun _ i _ ->
//               //   let combo, _ = combos.[i]
//               //   Some combo |> SetSelectedPair |> dispatch)
//               ]
//             []
//           Re.brush
//             [ Re.Props.Cartesian.StartIndex (float graphBrushStart)
//               Re.Props.Cartesian.EndIndex (min (pairs.Length - 1) graphBrushEnd |> float)
//              ] //Re.Props.Cartesian.OnChange !!indexChange ]
//             []
//           Re.xaxis
//             [ Re.Props.Cartesian.DataKey fst
//               Re.Props.Cartesian.Tick (pairImage combos)
//               Re.Props.Cartesian.Interval 0
//               Re.Props.Cartesian.Height 50.0 ]
//             [] ] ]









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

let pairImage model (pairs: (SeedId * FertilizerName option) array) selectPair props =
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
        svg.href <| Image.itemRoot (Model.getCrop model crop |> Crop.mainItem |> string)
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

let chartTooltip model (pairs: (SeedId * FertilizerName option) array) props =
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
    let (index: int, result: Result<float, Model.NoProfitReasons>) = payload[0]?payload
    let crop, fert = pairs[index]
    let fertDesc = Option.defaultOrMap "" (fun f -> " with " + string f)
    div [
      div (ofStr (Crop.name (Model.getItem model) (Model.getCrop model crop) + fertDesc fert))
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

let errorBar (pairs: (SeedId * FertilizerName option) array) props =
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
  | Error (flags: Model.NoProfitReasons) ->
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
        if flags.HasFlag Model.NoProfitReasons.NoFertilizerPrice then
          Svg.image [
            svg.className "pixel"
            svg.href <| Image.fertilizerRoot (string fert)
            svg.height width
          ]
        if flags.HasFlag Model.NoProfitReasons.NoSeeds then
          Svg.image [
            svg.className "pixel"
            svg.href <| Image.itemRoot (string crop)
            svg.height width
            svg.y width
          ]
        if flags.HasFlag Model.NoProfitReasons.NotEnoughDays then
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
          bar.dataKey (snd >> (function Ok y -> y | Error _ -> 0.0) >> (*) (if ranker.RankMetric = ROI then 100.0 else 1.0))
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

let allPairData metric timeNorm model =
  let crops =
    Model.selectedInSeasonCrops model |> sortByMany [|
      (fun c1 c2 ->
        match Crop.seasons c1, Crop.seasons c2 with
        | Seasons.None, Seasons.None -> 0
        | Seasons.None, _ -> 1
        | _, Seasons.None -> -1
        | s1, s2 -> Seasons.setOrder s1 s2)

      compareBy (Crop.name <| Model.getItem model)
    |]

  let fertilizers =
    let inline sort projection = compareBy (Option.map projection)
    Model.selectedFertilizersOpt model |> sortByMany [|
      sort Fertilizer.speed
      sort Fertilizer.quality
      sort Fertilizer.name
    |]

  let metric =
    match metric with
    | RankMetric.Gold -> Model.cropProfit
    | RankMetric.ROI -> Model.cropROISimple
    | RankMetric.XP -> Model.cropXP

  let data =
    crops |> Array.collect (fun crop ->
      let profit = metric model timeNorm crop
      fertilizers |> Array.map (fun fert ->
        (Crop.seed crop, Fertilizer.Opt.name fert), profit fert))

  {|
    Crops = crops |> Array.map Crop.seed
    Fertilizers = fertilizers |> Array.map Fertilizer.Opt.name
    Pairs = data
  |}

let rankBy labelText ranker dispatch =
  label [
    ofStr labelText
    selectUnitUnionWith (fun case ->
      option [
        text <| string case
        title (RankMetric.fullName case)
        valueOrDefault (Reflection.getCaseTag case)
      ] )
      RankMetric.all
      ranker.RankMetric
      (SetRankMetric >> dispatch)
    selectUnitUnion TimeNormalization.all ranker.TimeNormalization (SetTimeNormalization >> dispatch)
  ]

let graphView ranker model dispatch =
  let data = allPairData ranker.RankMetric ranker.TimeNormalization model
  if data.Pairs.Length = 0 then
    div [
      if data.Crops.Length = 0 then ofStr "No Crops Selected"
      if data.Fertilizers.Length = 0 then ofStr "No Fertilizers Selected"
    ]
  else
    let pairs =
      match ranker.RankItem with
      | RankCropsAndFertilizers -> data.Pairs
      | RankCrops -> data.Pairs |> Array.groupBy (fst >> fst) |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))
      | RankFertilizers -> data.Pairs |> Array.groupBy (fst >> snd) |> Array.map (snd >> Array.maxBy (snd >> Option.ofResult))

    let data =
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

    data |> Array.sortInPlaceWith (fun a b ->
      match snd a, snd b with
      | Ok a, Ok b -> compare b a
      | Ok _, Error _ -> -1
      | Error _, Ok _ -> 1
      | Error a, Error b -> setOrder a b)

    fragment [
      div [
        label [
          ofStr "Rank "
          selectUnitUnionWith (fun case ->
            let str =
              match case with
              | RankCropsAndFertilizers -> "All pairs of crops and fertilizers."
              | RankCrops -> "Pick the best fertilizer for each crop."
              | RankFertilizers -> "Pick the best crop for each fertilizer."
            option [
              text <| string case
              title str
              valueOrDefault (Reflection.getCaseTag case)
            ] )
            RankItem.all
            ranker.RankItem
            (SetRankItem >> dispatch)
        ]
        rankBy "By" ranker dispatch
      ]
      div [
        Class.graph
        children [
          lazyView3With
            (fun (data1, _) (data2, _) -> data1 = data2)
            (fun (pairs, data) (ranker, model) -> graph ranker model pairs data)
            (pairs, data)
            (ranker, model)
            dispatch
        ]
      ]
    ]

let growthCalender app seed fertilizer =
  let model = app.Model
  let crop = Model.getCrop model seed
  let fert = Model.getFertilizerOpt model fertilizer
  let growth = Crop.growth crop
  let spans = Model.growthSpans model crop
  let data = Growth.consecutiveHarvestsData spans (Model.growthSpeed model fert growth) growth

  let firstStageImages =
    data.Stages
    |> Array.mapi (fun i stage ->
      Array.create (int stage) (div [ Image.growthStage i seed ]))
    |> Array.concat
  let last =
    match crop with
    | FarmCrop _ -> Image.growthStage data.Stages.Length seed
    | ForageCrop _ -> Image.item' <| Crop.mainItem crop
  let last = [| div last |]
  let stageImages =
    let first =
      match growth.RegrowTime with
      | Some time -> Array.create (int time - 1) (div [ Image.regrowStage seed ])
      | None -> Array.tail firstStageImages
    Array.append first last
  let firstStageImages = Array.append firstStageImages last

  let disabledDay = div [ Class.disabled ]

  div [
    Class.calendar
    children [
      for i = 0 to spans.Length - 1 do
      let harvests = data.Harvests[i]
      if harvests > 0u then
        let span = spans[i]
        let unusedDays = span.TotalDays - nat firstStageImages.Length - (harvests - 1u) * nat stageImages.Length
        let days =
          [
            if model.StartDate.Season = span.StartSeason then
              Array.create (int (model.StartDate.Day - Date.firstDay)) disabledDay

            Array.create (int unusedDays) (div [])

            firstStageImages

            yield! Seq.replicate (int (harvests - 1u)) stageImages

            if model.EndDate.Season = span.EndSeason then
              Array.create (int (Date.lastDay - model.EndDate.Day)) disabledDay
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
// Initial Bought Seed                                         -> -gold   1 seed

// Each span:
//                          fertilizer (price)                 -> -gold
// item quality x amount -> product * quality (price) x amount -> gold
//                          item * quality (price) x amount    -> gold
// item quality x amount -> custom * quality (price) x amount  -> gold
//        itemA x amount ->
//        itemB x amount -> Forage Seeds (price) x amount      -> gold
//        itemC x amount ->
// item quality x amount -> seedmaker                          ->       +x seed
//                                       item quality x amount ->       +x seed
//        itemA x amount ->
//        itemB x amount -> Forage Seeds                       ->       +x seed
//        itemC x amount ->
//                 replacement fertilizer (price) x amount     -> -gold
//                           bought seeds (price) x amount     -> -gold +x seed
// Seeds Used                                                           -x seed
//                                                            Net: gold, 1 seed

//                                (if spans.Length > 1) Total Net: gold, 1 seed


// item amount -> item price amount -> gold seed

let profitBreakdownTable timeNorm model seed fertName =
  let crop = Model.getCrop model seed
  let fert = Model.getFertilizerOpt model fertName
  let items = Crop.items crop
  let data = Model.cropProfitData model timeNorm crop fert

  let seedsBoughtRow =
    let seedPrice = Model.seedLowestPriceBuyFrom model seed
    fun amount ->
      tr [
        td [ colSpan 4; children [
          Image.Icon.item' model (seed * 1<_>)
          match seedPrice with
          | Some (NonCustom vendor, _) ->
            ofStr " from "
            Image.Icon.vendor vendor
          | Some (Custom, _) ->
            ofStr " (Custom)"
          | None -> ofStr " from ???"
        ] ]
        td [
          match seedPrice with
          | Some (_, cost) -> gold cost |> ofStr
          | None -> ofStr "???"
        ]
        td [
          ofStr " x "
          ofStr <| floatFixedRound amount
        ]
        td [
          match seedPrice with
          | Some (_, cost) -> ofStr (goldFixedRound <| amount * -float cost)
          | None -> ofStr "???"
        ]
        td [
          if seedPrice.IsNone
          then none
          else ofStr <| floatFixedRound amount
        ]
      ]

  let fertilizerBoughtRow =
    match fert with
    | Some fert ->
      let fertPrice = Model.fertilizerLowestPriceBuyFrom model fert.Name
      fun amount ->
        tr [
          td [ colSpan 4; children [
            Image.Icon.fertilizer fert
            match fertPrice with
            | Some (NonCustom vendor, _) ->
              ofStr " from "
              Image.Icon.vendor vendor
            | Some (Custom, _) ->
              ofStr " (Custom)"
            | None -> ofStr " from ???"
          ] ]
          td [
            match fertPrice with
            | Some (_, cost) -> gold cost |> ofStr
            | None -> ofStr "???"
          ]
          td [
            ofStr " x "
            ofStr <| floatFixedRound amount
          ]
          td [
            match fertPrice with
            | Some (_, cost) -> ofStr (goldFixedRound <| amount * -float cost)
            | None -> ofStr "???"
          ]
          td []
        ]
    | None -> konst none

  table [ Class.breakdownTable; children [
    tbody [
      match model.SeedMode with
      | BuyFirstSeed -> seedsBoughtRow 1.0
      | StockpileSeeds ->
        tr [
          td [
            colSpan 6
            text "Previous Stockpiled Seed"
          ]
          td []
          td [
            ofStr "1 seed"
          ]
        ]
      | IgnoreSeeds -> none
    ]

    yield! data.ConsecutiveHarvests |> Array.map (fun harvestData ->
      tbody [
        tr [
          td [ colSpan 8; children [
            Image.Icon.season harvestData.StartSeason
            if harvestData.StartSeason <> harvestData.EndSeason then
              Image.rightArrow
              Image.Icon.season harvestData.EndSeason
            ofStr $" ({harvestData.Harvests} harvests)"
          ] ]
        ]

        fertilizerBoughtRow 1.0

        for i = 0 to items.Length - 1 do
          let item = items[i]
          let soldAmounts = harvestData.SoldAmounts[i]
          let sellAs = data.SellAs[i]

          for i = Quality.highest downto 0 do
            let quality = enum i
            let amount = soldAmounts[quality] * float harvestData.Harvests
            if amount = 0.0 then none else
            match sellAs with
            | Some sellAsData ->
              let sellAs, data = sellAsData[int quality]
              tr [
                if sellAs = NonCustom None then
                  td []
                  td []
                  td []
                else
                  td [
                    Image.Icon.itemQuality' model item quality
                  ]
                  td [
                    Html.span [
                      ofStr " x "
                      ofStr <| floatFixedRound amount
                    ]
                  ]
                  td [ div [ className "temp"; children Image.rightArrow ] ]

                td [
                  match sellAs with
                  | NonCustom None -> Image.Icon.itemQuality' model item quality
                  | NonCustom (Some product) ->
                    Image.Icon.productQuality model item product data.Quality
                  | Custom -> Html.span [ ofStr "Custom" ]
                ]
                td [
                  Html.span [
                    ofStr <| gold data.Price
                  ]
                ]
                td [
                  Html.span [
                    ofStr " x "
                    ofStr <| floatFixedRound (amount * data.Amount)
                  ]
                ]
                td [
                  Html.span [
                    ofStr <| goldFixedRound (float data.Price * data.Amount * amount)
                  ]
                ]
                td []
              ]
            | None ->
                tr [
                  td [
                    Image.Icon.itemQuality' model item quality
                  ]
                  td [
                    Html.span [
                      ofStr " x "
                      ofStr <| floatFixedRound amount
                    ]
                  ]
                  // td [ Image.rightArrow ]
                  td [ className "temp"; children Image.rightArrow ]
                  td [ colSpan 3; children [ ofStr "???" |> Html.span ] ]
                  td []
                  td []
                ]

        yield! harvestData.SeedAmounts |> Block.map' (fun (item, amounts) ->
          if int item = int seed then
            fragment [
              for i = Quality.highest downto 0 do
              let quality = enum i
              let amount = amounts[quality] * float harvestData.Harvests
              if amount = 0.0 then none else
              tr [
                td []
                td []
                td []
                td [
                  colSpan 2
                  children [
                    Image.Icon.itemQuality' model item quality
                  ]
                ]
                td [
                  Html.span [
                    ofStr " x "
                    ofStr <| floatFixedRound amount
                  ]
                ]
                td []
                td [
                  Html.span [
                    ofStr <| floatFixedRound amount
                  ]
                ]
              ]
            ]
          elif item = Crop.mainItem crop then
            fragment [
              for i = Quality.highest downto 0 do
              let quality = enum i
              let amount = amounts[quality] * float harvestData.Harvests
              if amount = 0.0 then none else
              tr [
                td [
                  Image.Icon.itemQuality' model item quality
                ]
                td [
                  Html.span [
                    ofStr " x "
                    ofStr <| floatFixedRound amount
                  ]
                ]
                // td [ Image.rightArrow ]
                td [ className "temp"; children Image.rightArrow ]
                td [
                  colSpan 2;
                  children [
                    Image.processor Processor.seedMaker
                    Image.Icon.productQuality model item (SeedsFromSeedMaker (seed * 1<_>)) Quality.Normal
                  ]
                ]
                td [
                  Html.span [
                    ofStr " x "
                    ofStr <| floatFixedRound (amount * Processor.seedMakerAmount (seed * 1<_>))
                  ]
                ]
                td []
                td [
                  Html.span [
                    ofStr <| floatFixedRound (amount * Processor.seedMakerAmount (seed * 1<_>))
                  ]
                ]
              ]
            ]
          else
            // failwith "what?"
            none)

        if harvestData.SeedsBought > 0.0 then
          let amount = harvestData.SeedsBought * (if Crop.regrowTime crop = None then float harvestData.Harvests else 1.0)
          let amount = amount - if model.SeedMode = BuyFirstSeed then 1.0 else 0.0
          if amount > 0.0 then
            seedsBoughtRow amount

        if harvestData.FertilizerBought > 1.0 then
          let amount = harvestData.FertilizerBought - 1.0
          fertilizerBoughtRow amount

        tr [
          yield! Seq.replicate 6 (td [])
          td [
            match data.NetProfit with
            | Some profit -> goldFixedRound profit |> ofStr
            | None -> none
          ]
          td []
        ]

        match data.NetProfit with
        | Some profit when timeNorm <> TotalPeriod ->
          tr [
            yield! Seq.replicate 6 (td [])
            td [
              ofStr "/ "
              match timeNorm with
              | PerDay ->
                data.TimeNormalization |> ofFloat
                ofStr " days"
              | PerSeason ->
                let value = floatRound data.TimeNormalization
                ofStr value
                ofStr (if value = "1" then " season" else " seasons")
              | TotalPeriod -> ofInt 1
            ]
            td []
          ]
          tr [
            yield! Seq.replicate 6 (td [])
            td [
              floatFixedRound (profit / data.TimeNormalization) |> ofStr
              match timeNorm with
              | TotalPeriod -> none
              | PerDay -> ofStr "g/day"
              | PerSeason -> ofStr "g/season"
            ]
            td []
          ]
        | _ -> none
      ] )
  ] ]


// Start Season
// End Season
// Harvests
// FertilizerBought (- 1.0 gives replacement fertilizer)
// SoldAmounts: Block<Qualities>
// ForageSeedsSold
// SeedAmounts: Block<ItemId * Qualities>
// ForageSeedsUsed
// SeedsBought: float
// NetProfit: float option

// SellAs
// TimeNormalization
// NetProfit?





// let selectedCropAndFertilizer app seed fert dispatch =
//   let appDispatch = dispatch
//   let dispatch = SetRanker >> dispatch
//   let model = app.Model
//   let ranker = app.Ranker

//   let data = allPairData ranker.RankMetric ranker.TimeNormalization model

//   let bestCrop, bestFert =
//     (match seed, fert with
//     | Some seed, None ->
//       data.Pairs |> Array.filter (fst >> fst >> (=) seed)
//     | None, Some fert ->
//       data.Pairs |> Array.filter (fst >> snd >> (=) fert)
//     | _ -> data.Pairs)
//     |> Array.maxBy (snd >> Option.ofResult)
//     |> fst

//   let effectiveSeed = seed |> Option.defaultValue bestCrop
//   let effectiveFert = fert |> Option.defaultValue bestFert

//   let crop = Model.getCrop app.Model effectiveSeed
//   // let fertilizer = fert |> Option.map (Model.getFertilizer model)
//   let harvests = Model.harvests model crop effectiveFert

//   let cropOptions =
//     let bestCrop = Model.getCrop model bestCrop
//     let bestName = Crop.name (Model.getItem model) bestCrop
//     data.Crops
//     |> Array.map (fun seed ->
//       let crop = (Model.getCrop model seed)
//       let name = Crop.name (Model.getItem model) crop
//       {| name = name
//          label = Html.span [ Image.crop crop; ofStr name ]
//          value = Some seed |})
//     |> Array.append [|
//       {| name = $"Best Crop { bestName }"
//          label = Html.span [ ofStr "Best Crop ("; Image.crop bestCrop ; ofStr $"{bestName})" ]
//          value = None |} |]

//   let fertilizerOptions =
//     let bestFertilizer = Model.getFertilizerOpt model bestFert
//     let bestName = bestFertilizer |> Option.defaultOrMap "No Fertilizer" Fertilizer.nameStr
//     data.Fertilizers
//     |> Array.map (fun name ->
//       {| name = name |> Option.defaultOrMap "No Fertilizer" string
//          label =
//           Html.span [
//             match name with
//             | Some name ->
//               Image.fertilizer' name
//               ofStr <| string name
//             | None -> ofStr "No Fertilizer" ]
//          value = Some name |})
//     |> Array.append [|
//       {| name = $"Best Fertilizer { bestName }"
//          label =
//           Html.span [
//             ofStr "Best Fertilizer ("
//             match bestFertilizer with
//             | Some fert -> Image.fertilizer fert
//             | None -> none
//             ofStr $"{bestName})" ]
//          value = None |} |]

//   div [
//     button [ onClick (fun _ -> SetSelectedCropAndFertilizer None |> dispatch); text "Back" ]

//     div [
//       reactSelect {|
//         options = cropOptions
//         value = cropOptions |> Array.find (fun opt -> opt.value = seed)
//         isSearchable = true
//         filterOption = fun opt input -> insensitiveLevenshteinMatch 0.4 opt?data?name input
//         onChange = fun opt -> SetSelectedCropAndFertilizer (Some (opt?value, fert)) |> dispatch
//       |}

//       ofStr " with "

//       reactSelect {|
//         options = fertilizerOptions
//         value = fertilizerOptions |> Array.find (fun opt -> opt.value = fert)
//         isSearchable = true
//         filterOption = fun opt input -> insensitiveLevenshteinMatch 0.4 opt?data?name input
//         onChange = fun opt -> SetSelectedCropAndFertilizer (Some (seed, opt?value)) |> dispatch
//       |}

//       if harvests = 1u
//       then ofStr $" ({harvests} harvest)"
//       else ofStr $" ({harvests} harvests)"

//       rankBy "showing" ranker dispatch
//     ]

//     if harvests = 0u then
//       div [
//         if Model.cropInSeason model crop
//         then ofStr "No Harversts Possible!"
//         else ofStr "Crop Not In Season!"
//       ]
//     else
//       animatedDetails [
//         isOpen (app.OpenDetails.Contains OpenDetails.RankerGrowthCalendar)
//         onToggle (curry SetDetailsOpen OpenDetails.RankerGrowthCalendar >> appDispatch)
//         children [
//           summary [ ofStr "Growth Calendar" ]
//           growthCalender app effectiveSeed effectiveFert ] ]
//       animatedDetails [
//         isOpen (app.OpenDetails.Contains OpenDetails.RankerProfitBreakdown)
//         onToggle (curry SetDetailsOpen OpenDetails.RankerProfitBreakdown >> appDispatch)
//         children [
//           summary [ ofStr "Profit Breakdown" ]
//           profitBreakdownTable ranker.TimeNormalization model effectiveSeed effectiveFert ] ]
//   ]

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
    let model = app.Model
    let ranker = app.Ranker

    let state = Fable.React.HookBindings.Hooks.useState (ranker.RankMetric, ranker.TimeNormalization)

    let data = allPairData (fst state.current) (snd state.current) model

    let bestCrop, bestFert =
      if data.Pairs.Length = 0 then None, None else
      let bestCrop, bestFert = data.Pairs |> Array.maxBy (snd >> Option.ofResult) |> fst
      match seed, fert' with
      | Some seed, None ->
        let filtered = data.Pairs |> Array.filter (fst >> fst >> (=) seed)
        Some bestCrop,
        (if filtered.Length = 0 then None else
          filtered
          |> Array.maxBy (snd >> Option.ofResult)
          |> fst
          |> snd
          |> Some)
      | None, Some fert_ ->
        let filtered = data.Pairs |> Array.filter (fst >> snd >> (=) fert_)
        (if filtered.Length = 0 then None else
          filtered
          |> Array.maxBy (snd >> Option.ofResult)
          |> fst
          |> fst
          |> Some),
        Some bestFert
      | _ -> Some bestCrop, Some bestFert


    let crop = seed |> Option.orElse bestCrop
    let fert = fert' |> Option.orElse bestFert
    let harvests =
      match crop, fert with
      | Some seed, Some fert_2 ->
        let crop = Model.getCrop app.Model seed
        let fert = Model.getFertilizerOpt app.Model fert_2
        Model.harvests model crop fert
      | _ -> 0u

    let cropOption seed =
      let crop = (Model.getCrop model seed)
      let name = Crop.name (Model.getItem model) crop
      {|
        name = name
        label = Html.span [ Image.crop crop; ofStr name ]
        value = Some seed
      |}

    let bestCropOption =
      let bestCrop = bestCrop |> Option.map (Model.getCrop model)
      let bestName = bestCrop |> Option.defaultOrMap "???" (Crop.name (Model.getItem model) )
      {|
        name = $"Best Crop { bestName }"
        label = Html.span [ ofStr "Best Crop ("; bestCrop |> Option.defaultOrMap (ofStr "???") Image.crop; ofStr $"{bestName})" ]
        value = None
      |}

    let cropOptions =
      data.Crops
      |> Array.map cropOption
      |> Array.append [| bestCropOption |]

    let bestFertilizerOption =
      let bestFertilizer = bestFert |> Option.map (Model.getFertilizerOpt model)
      let bestName = bestFertilizer |> Option.defaultOrMap "???" (Option.defaultOrMap "No Fertilizer" Fertilizer.nameStr)
      {|
        name = $"Best Fertilizer { bestName }"
        label = Html.span [
          ofStr "Best Fertilizer ("
          match bestFertilizer with
          | Some (Some fert) -> Image.fertilizer fert
          | _ -> none
          ofStr $"{bestName})"
        ]
        value = None
      |}

    let fertilizerOption name = {|
      name = name |> Option.defaultOrMap "No Fertilizer" string
      label = Html.span [
        match name with
        | Some name ->
          Image.fertilizer' name
          ofStr <| string name
        | None -> ofStr "No Fertilizer"
      ]
      value = Some name
    |}

    let fertilizerOptions =
      data.Fertilizers
      |> Array.map fertilizerOption
      |> Array.append [| bestFertilizerOption |]

    div [ Class.auditGraph; children [
      button [ onClick (fun _ -> SetSelectedCropAndFertilizer None |> dispatch); text "Back" ]

      div [ Class.auditGraphSelect; children [
        div [
          reactSelect {|
            className = "rselect"
            options = cropOptions
            value =
              seed |> Option.defaultOrMap bestCropOption (fun seed ->
                cropOptions
                |> Array.tryFind (fun opt -> opt?value |> Option.contains seed)
                |> Option.defaultValue (cropOption seed))
            isSearchable = true
            filterOption = createFilter {| stringify = fun opt -> opt?data?name |}
            onChange = fun opt -> SetSelectedCropAndFertilizer (Some (opt?value, fert')) |> dispatch
          |}

          ofStr " with "

          reactSelect {|
            className = "rselect"
            options = fertilizerOptions
            value = //fertilizerOptions |> Array.find (fun opt -> opt.value = fert')
              fert' |> Option.defaultOrMap bestFertilizerOption (fun fert ->
                fertilizerOptions
                |> Array.tryFind (fun opt -> opt?value |> Option.contains fert)
                |> Option.defaultValue (fertilizerOption fert))
            isSearchable = true
            onChange = fun opt -> SetSelectedCropAndFertilizer (Some (seed, opt?value)) |> dispatch
          |}
        ]

        // if harvests = 1u
        // then ofStr $" ({harvests} harvest)"
        // else ofStr $" ({harvests} harvests)"
        div [
          label [
            ofStr "Show "
            selectUnitUnionWith (fun case ->
              option [
                text <| string case
                title (RankMetric.fullName case)
                valueOrDefault (Reflection.getCaseTag case)
              ] )
              RankMetric.all
              (fst state.current)
              (fun metric -> state.update(fun (_, timeNorm) -> metric, timeNorm))
            selectUnitUnion TimeNormalization.all (snd state.current) (fun timeNorm -> state.update(fun (metric, _) -> metric, timeNorm))
          ]
        ]
      ] ]

      if harvests = 0u then
        div [
          if crop |> Option.exists (Model.getCrop model >> Model.cropInSeason model)
          then ofStr "No Harversts Possible!"
          else ofStr "Crop Not In Season!"
        ]
      else
        animatedDetails
          (app.OpenDetails.Contains OpenDetails.RankerProfitBreakdown)
          (ofStr "Profit Breakdown")
          [ Option.map2 (profitBreakdownTable (snd state.current) model) crop fert |> ofOption ]
          (curry SetDetailsOpen OpenDetails.RankerProfitBreakdown >> appDispatch)
        animatedDetails
          (app.OpenDetails.Contains OpenDetails.RankerGrowthCalendar)
          (ofStr "Growth Calendar")
          [ Option.map2 (growthCalender app) crop fert |> ofOption ]
          (curry SetDetailsOpen OpenDetails.RankerGrowthCalendar >> appDispatch)
    ] ] )

let compareModeView app dispatch =
  let appDispatch = dispatch
  let dispatch = SetRanker >> dispatch
  let ranker = app.Ranker
  match ranker.SelectedCropAndFertilizer with
  | Some (crop, fert) -> selectedCropAndFertilizer2 {| app = app; seed = crop; fert = fert; dispatch = appDispatch |}
  | None ->
    // div [
    //   id' "visualization-inner"
    //   children [ lazyView3 graphView app.Ranker app.Model dispatch ] ]
    lazyView3 graphView app.Ranker app.Model dispatch



let debug app =
  let model = app.Model
  // let farmingDistributions = model.Data.Fertilizers.Values |> Seq.map Some |> Seq.append [ None ] |> Seq.map (Model.farmingAmounts model) |> Array.ofSeq
  let crops = model.Data.Crops.Values |> Seq.choose (function | FarmCrop c -> Some c | ForageCrop _ -> None) |> Array.ofSeq
  // let time name func =
  //   JS.console.time name
  //   for _ = 1 to 100000 do
  //     for crop in crops do
  //       let profits = func model crop
  //       for dist in farmingDistributions do
  //         profits dist |> ignore
  //   JS.console.timeEnd name

  // //time "new" Model.farmCropNetProfit2
  // time "og" Model.farmCropNetProfit
  // JS.console.time "a"
  // for _ = 1 to 100000 do
  //   for crop in crops do
  //     let profits = Model.farmCropNetProfitPerHarvest model crop
  //     for dist in farmingDistributions do
  //       profits dist |> ignore
  // JS.console.timeEnd "a"
  // JS.console.time "b"
  // for _ = 1 to 100000 do
  //   for crop in crops do

  //     for dist in farmingDistributions do
  //       Model.farmCropNetProfitPerHarvest model crop dist |> ignore
  // JS.console.timeEnd "b"


  let data =
    Solver.solutionRequests
      app.Model
      (Model.selectedFertilizers model |> Seq.map Some |> Seq.append [ if model.AllowNoFertilizer then None ] |> Seq.map Fertilizer.Opt.name |> Array.ofSeq)
      (Model.selectedCrops model |> Seq.map Crop.seed |> Array.ofSeq)
  let solution, profit = Solver.solveRanges data
  // best, profit

  let x = [| 2.0; 4.0; 10.0; 401.0; 0.0 |]

  let inline time f x name =
      Fable.Core.JS.console.time name
      for _ = 1 to 100000 do
          f x |> ignore
      Fable.Core.JS.console.timeEnd name

  let reduceSum arr = arr?reduce(fun x y -> x + y)

  let forSum (arr: _ array) =
    let mutable sum = 0.0
    for i = 0 to arr?length - 1 do
      sum <- sum + arr[i]
    sum

  time Array.sum x "fable"
  time reduceSum x "native"
  time forSum x "native2" // wins by a landslide, 10-100x faster

  ()

let generateModeView app dispatch =

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
        app.Model
        (app.Model.SelectedFertilizers |> Seq.map Some |> Seq.append [ if app.Model.AllowNoFertilizer then None ] |> Array.ofSeq)
        (app.Model.SelectedCrops |> Array.ofSeq)
    for _ = 1 to n do
      data |> Solver.solveRanges |> ignore
    Fable.Core.JS.console.timeEnd name

  div [
    // selectionBox app dispatch

    // button [
    //   onClick (fun _ ->
    //     let r = System.Random()
    //     let model = app.Models.Current
    //     let newModel =
    //       { model with
    //           SelectedProducts =
    //             model.SelectedProducts |> Map.mapValues (fun set ->
    //               set |> Set.filter (fun _ -> r.Next(2) = 0)) }
    //     let app =
    //       { app with
    //           Models = { app.Models with Current = newModel } }
    //     debug app)
    //   text "Other" ]

    // button [
    //   onClick (fun _ -> debug ())
    //   text "debug" ]

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

    let model = app.Model
    let solution, total =
      Solver.solutionRequests
        model
        (model.SelectedFertilizers |> Seq.map Some |> Seq.append [ if model.AllowNoFertilizer then None ] |> Array.ofSeq)
        (model.SelectedCrops |> Array.ofSeq)
      |> Solver.solveRanges

    details [ children [
      summary ($"Total: {total}")
      div [
        Class.open'

        children (
          solution
          |> Seq.collect (fun res -> res.variables)
          |> Seq.map (fun (n, i) ->
            let varName =
              n.Split "@"
              |> Array.map (fun s ->
                match System.Int32.TryParse s with
                | true, value -> Model.getCrop model (value * 1<_>) |> Crop.name (Model.getItem model)
                | _ -> s)
              |> String.concat " "

            div (ofStr $"{varName}: {i}"))
        )
      ]
    ] ]
  ]


let view app dispatch =
  let rankerChart = app.AppMode = Ranker && app.Ranker.SelectedCropAndFertilizer.IsNone
  fragment [
    section [
      id' (if rankerChart then "visualization-graph" else "visualization")
      children [
        viewTabsCss [ Class.tabs ] SetAppMode AppMode.all app.AppMode dispatch
        match app.AppMode with
        | Ranker -> compareModeView app dispatch
        | Solver -> generateModeView app dispatch
      ]
    ]

    settings app dispatch
  ]


let viewWhole app dispatch =
  match app with
  | Ok app -> view app dispatch
  | Error e -> div (ofStr e)



open Elmish

let init () = Json.loadDefaultApp (), []

Program.mkProgram init (fun msg app ->
  match app with
  | Ok app ->
    let app, cmd = Update.app msg app
    Ok app, cmd
  | Error e -> Error e, [])
  viewWhole
|> Program.withReactBatched "app"
|> Program.run
