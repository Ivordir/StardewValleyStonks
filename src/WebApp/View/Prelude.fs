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
module Icon =
  module Path =
    let crop (seed: SeedId) (file: string) = $"img/Crops/{seed}/{file}.png"
    let at (folder: string) (file: string) = $"img/{folder}/{file}.png"
    let fertilizer = at "Fertilizers"
    let item (itemId: ItemId) = itemId |> string |> at "Items"
    let processor = at "Processors"
    let vendor = at "Vendors"

  let private fromClassNameAndAlt (name: string) altText =
    img [
      className (lowerCase name)
      alt altText
    ]

  let private fromClassName name = fromClassNameAndAlt name ""

  let private fromImageAndText img text =
    Html.span [ className "icon-text"; children [
      img
      ofStr text
    ]]

  let private fromClassAndName = fromClassName >> fromImageAndText

  let private fromClassIsName name = fromClassAndName name name

  let private fromPathAndAlt path altText =
    img [
      className "icon"
      src path
      alt altText
    ]

  let private fromPath path = fromPathAndAlt path ""

  let private fromPathAndName = fromPath >> fromImageAndText

  let growthStage (i: int) seed = fromPath (Path.crop seed (string i))
  let regrowStage seed = fromPath (Path.crop seed "Regrow")

  let rightArrow = fromClassName "arrow-right"
  let upArrow = fromClassName "arrow-up"
  let downArrow = fromClassName "arrow-down"

  let seasonNoText season =
    let name = Season.name season
    fromClassNameAndAlt name name

  let season = Season.name >> fromClassIsName

  let profession (profession: Profession) = profession |> string |> fromClassIsName
  let skill name = fromClassIsName name

  let itemIdNoText itemId = fromPath (Path.item itemId)
  let item item = fromPathAndName (Path.item item.Id) item.Name
  let itemId (data: GameData) itemId = item data.Items[itemId]
  let seed data seed = seed |> toItem |> itemId data

  let private qualities = Qualities.init (Quality.name >> fromClassName)

  let itemQuality item quality =
    let img =
      div [ Class.quality; children [
        fromPath (Path.item item.Id)
        qualities[quality]
      ]]

    fromImageAndText img item.Name

  let itemIdQuality (data: GameData) item quality = itemQuality data.Items[item] quality

  let product data product =
    match product with
    | SeedsFromSeedMaker item
    | Processed { Item = item } -> itemId data item
    | product -> fromClassAndName (Reflection.getCaseName product) (Product.name data.Items.Find product)

  let productQuality data product quality =
    match product with
    | SeedsFromSeedMaker item
    | Processed { Item = item } -> itemIdQuality data item quality
    | product ->
      let img =
        div [ Class.quality; children [
          product |> Reflection.getCaseName |> fromClassName
          qualities[quality]
        ]]

      fromImageAndText img (Product.name data.Items.Find product)

  let fertilizerName name = fromPathAndName (Path.fertilizer name) name
  let fertilizer = Fertilizer.name >> fertilizerName

  let crop data = function
    | FarmCrop crop -> crop.Item |> itemId data
    | ForageCrop crop -> fromPathAndName (crop.Seed |> toItem |> Path.item) (ForageCrop.name crop)

  let vendorNoText (VendorName name) = fromPathAndAlt (Path.vendor name) name
  let vendor (VendorName name) = fromPathAndName (Path.vendor name) name

  let private withClass css path name =
    fragment [
      img [ css; src path; alt "" ]
      ofStr name
    ]

  let processor = function
    | ProcessorName "Mill" -> withClass Class.iconProcessorLarge (Path.processor "Mill") "Mill"
    | ProcessorName processor -> withClass Class.iconProcessor (Path.processor processor) processor



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

