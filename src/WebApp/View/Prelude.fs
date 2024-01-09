[<AutoOpen>]
module StardewValleyStonks.WebApp.View.Prelude

open StardewValleyStonks
open StardewValleyStonks.WebApp
open StardewValleyStonks.WebApp.Update

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
let gold2 gold = sprintf "%.2fg" gold

let xpFloat (xp: float) = string xp + "xp"
let xp (xp: nat) = string xp + "xp"

let percent value = sprintf "%.0f%%" (value * 100.0)
let percent2 value = sprintf "%.2f%%" (value * 100.0)

let inline lowerCase (str: string) = str.ToLower ()
let inline strContains (text: string) (str: string) = str.Contains text
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

let clampIndex array index = index |> min (Array.length array - 1) |> max 0

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

  let growthStage seed (item: Item) (stage: int) =
    img [
      alt $"{item.Name} Stage {stage}"
      src (Path.crop seed (string stage))
    ]

  let regrowStage seed (item: Item) =
    img [
      alt $"{item.Name} Regrow Stage"
      src (Path.crop seed "Regrow")
    ]

  let private fromClassNameAndAlt (name: string) altText =
    img [
      className (lowerCase name)
      alt altText
    ]

  let private fromClassName name = fromClassNameAndAlt name ""

  let private messageWithChildren (name: string) (children: ReactElement) =
    Html.span [ className Class.iconMessage; prop.children [
      fromClassName name
      Html.span children
    ]]

  let warningWith children = messageWithChildren "warning" children
  let errorWith children = messageWithChildren "error" children

  let private message name text = messageWithChildren name (ofStr text)

  let warning text = message "warning" text
  let error text = message "error" text

  let private fromImageAndText img text =
    Html.span [ className Class.iconText; children [
      img
      ofStr text
    ]]

  let private fromClassAndName = fromClassName >> fromImageAndText

  let private fromClassIsName name = fromClassAndName name name

  let private fromPathAndAlt path altText =
    img [
      className Class.icon
      src path
      alt altText
    ]

  let private fromPath path = fromPathAndAlt path ""

  let private fromPathAndName = fromPath >> fromImageAndText

  let season = Season.name >> fromClassIsName

  let profession (profession: Profession) = profession |> Reflection.getCaseName |> fromClassIsName
  let farming = fromClassIsName "Farming"
  let foraging = fromClassIsName "Foraging"

  let bearsKnowledge = fromClassAndName "bears-knowledge" "Bear's Knowledge"
  let specialCharm = fromClassAndName "special-charm" "Special Charm"
  let luckBuff = fromClassAndName "random" "Luck Buff"

  let item item = fromPathAndName (Path.item item.Id) item.Name
  let itemId data itemId = item data.Items[itemId]
  let seed data seed = seed |> toItem |> itemId data

  let private withQuality quality name img =
    let qualityName = Quality.name quality
    let alt =
      if quality = Quality.Normal
      then ""
      else $"{qualityName} quality"

    let img =
      div [ className Class.quality; children [
        img
        fromClassNameAndAlt qualityName alt
      ]]

    fromImageAndText img name

  let itemQuality item quality = item.Id |> Path.item |> fromPath |> withQuality quality item.Name
  let itemIdQuality data item quality = itemQuality data.Items[item] quality

  let productQuality data product quality =
    match product with
    | SeedsFromSeedMaker item
    | Processed { Item = item } -> itemIdQuality data item quality
    | product ->
      product
      |> Reflection.getCaseName
      |> fromClassName
      |> withQuality quality (Product.name data.Items.Find product)

  let fertilizerName name = fromPathAndName (Path.fertilizer name) name
  let fertilizer (fertilizer: Fertilizer) = fertilizerName fertilizer.Name

  let crop data = function
    | FarmCrop crop -> crop.Item |> itemId data
    | ForageCrop crop -> crop.Seed |> seed data

  let vendor name = fromPathAndName (Path.vendor name) name

  let processor processor =
    fromImageAndText
      (img [
        className Class.iconProcessor
        src (Path.processor processor)
        alt ""
      ])
      processor

  module NoText =
    let season season =
      let name = Season.name season
      fromClassNameAndAlt name name

    let item item = fromPathAndAlt (Path.item item.Id) item.Name
    let itemId data itemId = item data.Items[itemId]

    let vendor name = fromPathAndAlt (Path.vendor name) name

    let processor processor =
      img [
        className Class.iconProcessor
        src (Path.processor processor)
        alt processor
      ]


let labelWith (css: string) (label: ReactElement) element =
  Html.label [
    Html.span [ className css; children label ]
    element
  ]

let labelText className text element = labelWith className (ofStr text) element

let labeled label element = labelText Class.labelText label element
let labeledHidden label element = labelText Class.labelHidden label element


let [<ReactComponent>] private LazyInit (props: {|
    Visible: bool
    Content: unit -> ReactElement
  |}) =
  let openedOnce, setOpenedOnce = useState props.Visible

  if props.Visible && not openedOnce then
    setOpenedOnce true

  if openedOnce
  then props.Content ()
  else none

let lazyInit visible element = LazyInit {|
  Visible = visible
  Content = element
|}


// https://www.w3.org/TR/wai-aria-1.1/#tab

let private tabId label tab = $"{lowerCase label}-tab-{tab |> Reflection.getCaseName |> lowerCase}"

let [<ReactComponent>] Tabs (props: {|
    Tabs: 'a array
    Label: string
    Current: 'a
    Dispatch: 'a -> unit
    Panel: 'a -> ReactElement
  |}) =
  let tabs = props.Tabs
  let current = props.Current
  let dispatch = props.Dispatch
  let label = props.Label

  let tabRefs = useRef (Array.create tabs.Length (None: Browser.Types.HTMLElement option))

  fragment [
    div [
      role "tablist"
      ariaLabel $"{label} Tabs"
      children (tabs |> Array.mapi (fun i tab ->
        let active = current = tab
        button [
          role "tab"
          prop.id (tabId label tab)
          Interop.mkAttr "ref" (Array.set tabRefs.current i)
          ariaSelected active
          tabIndex (if active then 0 else -1)
          onClick (fun _ -> dispatch tab)
          onKeyDown (fun e ->
            (match e.key with
            | "ArrowRight" -> Some (if i = tabs.Length - 1 then 0 else i + 1)
            | "ArrowLeft" -> Some (if i = 0 then tabs.Length - 1 else i - 1)
            | _ -> None)
            |> Option.iter (fun i ->
              tabRefs.current[i] |> Option.iter (fun elm -> elm.focus ())
              dispatch tabs[i]))

          text (Reflection.getCaseName tab)
        ]))
    ]
    div [
      role "tabpanel"
      ariaLabelledBy (tabId label current)
      children (tabs |> Array.map (fun tab ->
        let active = tab = current
        div [
          prop.id $"{lowerCase label}-{tab |> Reflection.getCaseName |> lowerCase}"
          if active then className Class.active
          children (lazyInit active (fun _ -> props.Panel tab))
        ]
      ))
    ]
  ]

let inline tabs label current dispatch tabpanel =
  Tabs {|
    Tabs = unitUnionCases
    Label = label
    Current = current
    Dispatch = dispatch
    Panel = tabpanel
  |}

let private detailsSectionWith openDetails key (summaryContent: ReactElement) (children: ReactElement) dispatch =
  let open' = openDetails |> Set.contains key
  let id = $"{key |> Reflection.getCaseName |> lowerCase}-details"
  details [
    isOpen open'
    onToggle (curry SetDetailsOpen key >> dispatch)
    prop.children [
      summary summaryContent
      div [
        prop.id id
        prop.children children
      ]
    ]
  ]

let detailsSection openDetails key summary children dispatch =
  detailsSectionWith openDetails key summary children dispatch

let lazyDetails openDetails key summary children dispatch =
  detailsSectionWith openDetails key summary (lazyInit (openDetails |> Set.contains key) children) dispatch

