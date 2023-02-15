[<AutoOpen>]
module StardewValleyStonks.WebApp.View.Prelude

open StardewValleyStonks
open StardewValleyStonks.WebApp

open Fable.Core.JsInterop
open Feliz

type prop with
  static member inline onToggle (handler: bool -> unit) =
    Interop.mkAttr "onToggle" (fun (e: Browser.Types.Event) -> handler e.target?``open``)


type Html with
  static member inline text (n: nat) = string n |> Html.text


let ofOption mapping option = option |> Option.defaultOrMap Html.none mapping
let inline ofStr (str: string) = Html.text str
let inline ofNat (n: nat) = Html.text n
let inline ofInt (i: int) = Html.text i
let inline ofFloat (x: float) = Html.text x

let round2 (x: float) = System.Math.Round (x, 2)

let gold (g: nat) = string g + "g"
let gold2 = sprintf "%.2fg"

let xpFloat (xp: float) = string xp + "xp"
let xp (xp: nat) = string xp + "xp"

let percent value = sprintf "%.0f%%" (value * 100.0)
let percent2 value = sprintf "%.2f%%" (value * 100.0)

let inline lowerCase (str: string) = str.ToLower ()
let upperFirstChar (text: string) = text[0..0].ToUpper() + text[1..]

let pluralize text = text + "s"
let pluralizeTo value text =
  if value = 1u
  then text
  else pluralize text

let debouncer timeout (f : _ -> unit) =
  let mutable last = None
  fun value ->
    last |> Option.iter Browser.Dom.window.clearInterval
    last <- Some (Browser.Dom.window.setTimeout ((fun () -> f value), timeout))

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
      at (qualityRoot (Quality.name quality))
    ]]
  let itemQuality' = item' >> withQuality
  let crop = function
    | FarmCrop crop -> at (crop.Item |> string |> itemRoot)
    | ForageCrop crop -> at (crop.Seed |> string |> itemRoot)
  let growthStage (i: int) (seed: SeedId) = at (cropRoot $"{seed}/{i}")
  let regrowStage (seed: SeedId) = at (cropRoot $"{seed}/Regrow")

  let fertilizer' (fertilizer: FertilizerName) = fertilizer |> fertilizerRoot |> at
  let fertilizer = Fertilizer.name >> fertilizer'
  let skill = skillRoot >> at
  let profession (profession: Profession) = profession |> string |> skill
  let vendor (VendorName vendor) = vendor |> vendorRoot |> at
  let processor = function
    | ProcessorName "Mill" -> "Mill" |> processorRoot |> withClass Class.iconProcessorLarge
    | ProcessorName processor -> processor |> processorRoot |> withClass Class.iconProcessor

  let private productPath = function
    | Jam _ -> productRoot (nameof Jam)
    | Pickles _ -> productRoot (nameof Pickles)
    | Wine _ -> productRoot (nameof Wine)
    | Juice _ -> productRoot (nameof Juice)
    | SeedsFromSeedMaker seed -> itemPath seed
    | Processed product -> itemPath product.Item

  let product = function
    | SeedsFromSeedMaker seed -> item' seed
    | Processed product -> item' product.Item
    | product -> at (productPath product)

  let productQuality = product >> withQuality

  let allQualities = at (qualityRoot "All")
  let rightArrow = img [ className "arrow-right"; alt "" ]
  let upArrow = img [ className "arrow-up"; alt "" ]
  let downArrow = img [ className "arrow-down"; alt "" ]


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

    let profession (profession: Profession) = profession |> string |> skill


    let fertilizerName = nameIsPartofPath fertilizerRoot
    let fertilizer = Fertilizer.name >> fertilizerName

    let item (item: Item) = at (itemPath item.Id) item.Name
    let itemId (data: GameData) = data.Items.Find >> item
    let seed data (seed: SeedId) = seed |> convertUnit |> itemId data

    let private withQuality path name quality =
      fragment [
        withQuality (img [ src path ]) quality
        ofStr name
      ]

    let itemQuality item quality = withQuality (itemPath item.Id) item.Name quality

    let itemQuality' (data: GameData) = data.Items.Find >> itemQuality

    let crop (data: GameData) = function
      | FarmCrop crop -> at (crop.Item |> string |> itemRoot) (FarmCrop.name data.Items.Find crop)
      | ForageCrop crop -> at (crop.Seed |> string |> itemRoot) (ForageCrop.name crop)

    let vendor (VendorName name) = nameIsPartofPath vendorRoot name

    let processor = function
      | ProcessorName "Mill" -> withClass Class.iconProcessorLarge (processorRoot "Mill") "Mill"
      | ProcessorName processor -> withClass Class.iconProcessor (processorRoot processor) processor

    let product (data: GameData) product =
      let name = Product.name data.Items.Find product
      let path = productPath product
      at path name

    let productQuality (data: GameData) product quality =
      let name = Product.name data.Items.Find product
      let path = productPath product
      withQuality path name quality

    let season = Season.name >> nameIsPartofPath seasonRoot


let labeled label element =
  Html.label [
    ofStr label
    element
  ]


let viewTab toString tab currentTab dispatch =
  li [
    if currentTab = tab then Class.active
    children [
      button [
        onClick (fun _ -> dispatch tab)
        text (toString tab: string)
      ]
    ]
  ]

let viewTabsWith toString tabs currentTab dispatch =
  ul [ Class.tabs; children (tabs |> Array.map (fun tab ->
    viewTab toString tab currentTab dispatch))
  ]

let inline viewTabs current dispatch =
  viewTabsWith Reflection.getCaseName unitUnionCases current dispatch


let animatedDetails open' (summary': ReactElement) (children': ReactElement) dispatch =
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

