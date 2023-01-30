[<AutoOpen>]
module StardewValleyStonks.WebApp.View.Prelude

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Fable.Core.JsInterop
open Feliz

type prop with
  static member inline onToggle (handler: bool -> unit) =
    Interop.mkAttr "onToggle" (fun (e: Browser.Types.Event) -> handler e.target?``open``)

  static member inline onChange (handler: nat -> unit) =
    Interop.mkAttr "onChange" (fun (e: Browser.Types.Event) ->
      let value: float = unbox e.target?valueAsNumber
      if not <| isNullOrUndefined value
        && not <| System.Double.IsNaN value
        && value >= 0.0
      then
        round value |> unbox |> handler)

  static member inline valueOrDefault (n: nat) =
    prop.ref (fun e -> if e |> isNull |> not && e?value <> n then e?value <- n)

  static member inline value (n: nat) = Interop.mkAttr "value" n
  static member inline min (n: nat) = Interop.mkAttr "min" n
  static member inline max (n: nat) = Interop.mkAttr "max" n


type Html with
  static member inline text (n: nat) = string n |> Html.text


let inline ofStr (str: string) = Html.text str
let inline ofNat (n: nat) = Html.text n
let inline ofInt (i: int) = Html.text i
let inline ofFloat (x: float) = Html.text x


let debouncer timeout (f : _ -> unit) =
  let mutable last = None
  fun value ->
    last |> Option.iter Browser.Dom.window.clearInterval
    last <- Some <| Browser.Dom.window.setTimeout ((fun () -> f value), timeout)

let internal handleEvent (event: Browser.Types.Event) =
  event.stopPropagation ()
  event.preventDefault ()



open Fable.Core

open type Html
open type prop
open type React

open Core.Operators
open Core.ExtraTopLevelOperators

[<RequireQualifiedAccess>]
module Class =
  let checkboxLabel = className "checkbox-label"
  let checkboxImg = className "checkbox-img"
  let iconProcessor = className "icon-processor"
  let iconProcessorLarge = className "icon-processor-large"

  let fileInput = className "file-input"

  let active = className "active"
  let hover = className "hover"
  let disabled = className "disabled"
  let open' = className "open"
  let tabs = className "tabs"

  let select = className "select"
  let selectControl = className "select-control"
  let selectList = className "select-list"
  let selectListHidden = className "select-list-hidden"
  let selectInput = className "select-input"
  let selectInputHidden = className "select-input-hidden"

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


[<RequireQualifiedAccess>]
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
  let regrowStage (seed: SeedId) =
    at <| cropRoot $"{seed}/Regrow"

  let fertilizer' (fertilizer: FertilizerName) = fertilizer |> fertilizerRoot |> at
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

  [<RequireQualifiedAccess>]
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
let floatRound (x: float) = System.Math.Round (x, 2)
let goldFixedRound = sprintf "%.2fg"


let opacityCheckbox checked' dispatch =
  label [
    className "qualities-checkbox"
    children [
      input [
        prop.type'.checkbox
        isChecked checked'
        onCheckedChange dispatch
      ]
      img []
    ]
  ]

let checkboxWith children' checked' dispatch =
  label [
    className "checkbox-label"
    onClick (fun e -> e.stopPropagation ())
    children [
      input [
        prop.type'.checkbox
        isChecked checked'
        onCheckedChange dispatch
      ]
      img []
      children'
  ] ]

let checkbox = checkboxWith none
let inline checkboxText str checked' msg = checkboxWith (ofStr str) checked' msg


let labeled label element =
  Html.label [
    ofStr label
    element
  ]


let viewTab toString tab currentTab dispatch =
  li [
    if currentTab = tab then Class.active
    children (
      button [
        onClick (fun _ -> dispatch tab)
        text (toString tab: string)
      ] )
  ]

let viewTabsWith toString tabs currentTab dispatch =
  ul [ Class.tabs; children (tabs |> Array.map (fun tab ->
    viewTab toString tab currentTab dispatch))
  ]

let inline viewTabs (current: 'tab) dispatch = viewTabsWith Reflection.getCaseName unitUnionCases<'tab> current dispatch


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

