module StardewValleyStonks.App

open Fable.React
open Fable.React.Props
open Browser

open FSharp.Data.Adaptive
open Fable.React.Adaptive

open Adaptive
open Util

type SettingsTab =
  | Skills
  | Crops
  | Fertilizers
  | Date
  | Misc
  | Mod

let settingsTabs =
  [ Skills
    Crops
    Fertilizers
    Date
    Misc
    Mod ]
let settingsTab = cval Skills

let fertilizerSort = cval Speed
let fertilizerSortAscending = cval true

type CropTab =
  | Growth
  | SeedPrices
  | Products
  | Replants
let cropTabs =
  [ Growth
    SeedPrices
    Products
    Replants ]
let cropTab = cval Growth

let cropSort = cval CropName
let cropSortAscending = cval true

let cropSelection: Crop cset = ChangeableHashSet(Crop.all)
let fertilizerSelection = ChangeableHashSet(Fertilizers.allOption)

//let combos = 

let showUnprofitableCombos = cval false
let showInvalidCombos = cval false
let selectedCombo: (Crop * Fertilizer option) option cval = cval None







let private typeCheckbox = Type "checkbox"
let private typeRange = Type "range"
let private typeNumber = Type "number"

let pseudoNothing = div [] []
let aNothing = !@nothing
let noAttr = AttributeMap.empty

let aOfInt: int aval -> _ = aString >> astr

let aClassBaseList baseClass = AList.ofList >> AList.filterA snd >> AList.map fst >> AList.fold (fun state name -> state + " " + name) baseClass >> !%ClassName
let aClassBase baseClass single add = aClassBaseList baseClass [ single, add ]
let aClassList = aClassBaseList ""

let onEvent event (change: unit -> unit) = event <| fun _ -> change()
let onChange = onEvent OnChange
let onClick = onEvent OnClick

let condAttr attr = aCond (Some attr) None
let cssOption = defaultMap [] (ClassName >> List.singleton)

let gold = sprintf "%ig"
let percent v = sprintf "%f%%" (v * 100.0)
let percent2 v = sprintf "%.2f%%" (v * 100.0)

module Image =
  let path = sprintf "img/%s/%s.png"
  let profession = path "Skills"
  let skill = path "Skills"
  let fertilizer = path "Fertilizers"
  let crop = path "Crop"
  let source = path "BuyAndSell"
  let processor = path "BuyAndSell"

  let at path = img [ Src <| path ]

let image imgPath = imgPath >> Image.at

let imageAndName imgPath name =
  [ Image.at <| imgPath name
    str name ]

let checkboxCssWith change css alsoDisplay isChecked =
  label css
    ( [ ainput (attr {
          typeCheckbox
          !%Checked isChecked
          onChange change } )
        aimg (attr {
          !%Src (isChecked |> aCond
              "img/UI/CheckboxGreen.png"
              "img/UI/Checkbox.png") } ) ]
      @ alsoDisplay)

let checkboxCss css alsoDisplay isChecked = checkboxCssWith (toggleDelay isChecked) css alsoDisplay isChecked

let checkboxWith = checkboxCss [ ClassName "checkbox-label" ]

// let checkboxDisabledWith alsoDisplay disabled =
//   checkboxCss (classBase "checkbox-label" "disabled" disabled) alsoDisplay

// let checkboxDisabled = checkboxDisabledWith []

// let checkboxDisabledText text = checkboxDisabledWith [ str text ]

let checkbox = checkboxWith []

let checkboxText text = checkboxWith [ str text ]

let viewTabWith toString toggle active tab =
  ali (attr {
    active |> condAttr (ClassName "active")
    onClick toggle } )
    (tab |> toString |> str |> AList.single)

let inline viewTab currentTab tab =
  viewTabWith string (setValueDelay currentTab tab) (!@tab .= currentTab) tab

let inline viewTabsWith attr tabs currentTab =
  ul attr
    (tabs |> List.map (viewTab currentTab))
let inline viewTabsCss css = viewTabsWith [ ClassName css ]
let inline viewTabs tabs = viewTabsWith [] tabs



let viewDistribution dist =
  div [ ClassName "quality-distribution" ] (dist |> Map.toList |> List.map (fun (quality, prob) ->
    adiv (attr {
        prob |> AVal.map (fun x -> Style [ Width (percent x) ] )
        Style [ BackgroundColor (Quality.color quality) ] } )
      (prob |> !%percent2 |> astr |> AList.single)))

let setInt value validate (event: Types.Event) = setValue value (event.Value |> System.Int32.Parse |> validate)

let skillLevelInput level =
  label []
    [ str "Level:"
      let setLevel = OnChange <| setInt level Skill.validLevel
      let value = !&Value level
      let min0 = Min 0
      let max10 = Max 10
      ainput (attr {
        typeRange
        min0
        max10
        value
        setLevel } )
      ainput (attr {
        typeNumber
        min0
        max10
        value
        setLevel } ) ]

let skillBuffInput buff =
  label []
    [ str "Buff:"
      ainput (attr {
        typeNumber
        Min 0
        !&Value buff
        OnChange <| setInt buff Skill.validBuff } ) ]

let overrideCheckbox over value =
  checkboxCssWith (setValueDelay over value) [ ClassName "checkbox-label" ] [] (over .= !@value)

let overrideInput over =
  div []
    [ overrideCheckbox over (Some false)
      overrideCheckbox over (None)
      overrideCheckbox over (Some true) ]

let warningIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Warning.png" ]
let errorIcon =
  img
    [ ClassName "alert"
      Src "img/UI/Error.png" ]

let viewProfession (profession: Profession) toggle =
  abutton (attr {
      aClassList
        [ "active", !>profession.Selected
          "disabled", aNot profession.Active ]
      onClick <| toggle profession } )
    (alist {
      let! warn = profession.Selected .&& aNot profession.Unlocked .&& (Settings.skillLevelPolicy .= !@Warn)
      if warn then warningIcon
      yield! imageAndName Image.profession profession.Name } )

let viewSkill (skill: Skill) =
  li []
    [ span []
        (imageAndName Image.skill skill.Name)
      skillLevelInput skill.Level
      skillBuffInput skill.Buff
      div [ ClassName "professions" ]
        [ let toggle = Profession.toggle skill
          div []
            [ viewProfession skill.Lvl5Profession toggle ]
          div []
            [ viewProfession skill.Lvl10ProfessionA toggle
              match skill.Lvl10ProfessionB with
              | Some p -> viewProfession p toggle
              | None -> nothing ] ]
      viewDistribution skill.QualityDistribution ]

let private selectSeasons =
  (Seasons.all |> List.map (fun season ->
    option [ Value (int season) ]
      [ str <| Season.name season ] ))
let dateInput text (date: DateInput) =
  div []
    [ str <| text + ":"
      select
        [ OnChange <| setInt date.Season enum<Season>
          Value (int date.Season.Value) ]
        selectSeasons
      ainput (attr {
        typeNumber
        Min 1
        Max 28
        !&Value date.Day
        OnChange <| setInt date.Day DateInput.validDay } ) ]

let viewTable css columns toKey items =
  table [ ClassName css ]
    [ colgroup [] (columns |> List.map (fun (width, _,_) ->
        col [ Style [ Width (percent width) ] ] ))
      thead []
        [ tr []
            (columns |> List.map sndOf3) ]
      tbody []
        [ ofList
            (items |> List.map (fun item ->
              tr [ Key <| toKey item ]
                (columns |> List.map (lastOf3 >> (|>) item)))) ] ]

let colWidths widths =
  colgroup [] (widths |> List.map (fun width ->
    col [ Style [ Width (percent2 width) ] ] ))

let headerSort changeSort sort text =
  th [ onClick <| changeSort sort ]
    [ str text ]

let fertRow = memoize <| (fun fert ->
  (fun cols ->
    let name = Fertilizer.name fert
    tr [ Key name ] //onclick to open more info/edit?
      [ td []
          (imageAndName Image.fertilizer fert.Name)
        yield! cols ] ))

let viewFert = memoize <| (fun fert ->
  fertRow fert
    [ td []
        [ fert.BestPrice |> aDefaultMap "No Prices" string |> astr ]
      td []
        [ ofFloat fert.Speed ]
      td []
        [ viewDistribution fert.Distribution ] ] )

let priceTable typeName image itemName bestPrice bestSources prices items sources =
  table [ ClassName "price-table" ]
    [ colWidths [ 0.2; 0.2 ]
      thead []
        [ tr []
            [ th []
                [ str typeName ]
              th []
                [ str "Lowest Price" ]
              yield! sources |> List.map (fun (source, selected) -> 
                th []
                  [ yield! imageAndName Image.source source
                    checkbox selected ] ) ] ]
      tbody [] (items |> List.map (fun item ->
        let name = itemName item
        tr [ Key name ]
          [ td []
              (imageAndName image name)
            td []
              [ bestPrice item |> aDefaultMap "No Prices" gold |> astr
                aul noAttr
                  (bestSources item |> AList.map (Image.source >> Image.at >> List.singleton >> li [])) ]
            yield! sources |> List.map (fun (source, _) ->
              atd noAttr (alist {
                match prices item |> Map.tryFind source with
                | Some price ->
                    price |> Price.value |> !%gold |> astr
                    overrideInput <| Price.sourceOverride price
                | None -> nothing } )) ] )) ]

type Page =
  | Home
  | Stonks
  | Planner
  | Help

module Page =
  let url page =
    match page with
    | Home -> ""
    | page -> string page
    |> sprintf "#%s"

  //basic parsing, case-sensitive
  let parse = function
    | "" | "Home" -> Some Home
    | "Stonks" -> Some Stonks
    | "Planner" -> Some Planner
    | "Help" -> Some Help
    | _ -> None

let page = cval Home

let ui =
  div [ ClassName "app" ]
    [ adiv noAttr
        (alist {
          adaptive {
            match! page with
            | Home -> return "h"
            | Stonks -> return "s"
            | Planner -> return "p"
            | Help -> return "help!" }
          |> astr

          a [ Href <| Page.url Home ]
            [ str "Home" ]
          a [ Href <| Page.url Stonks ]
            [ str "Stonks" ]
          a [ Href <| Page.url Planner ]
            [ str "Planner" ]
          a [ Href <| Page.url Help ]
            [ str "Help" ] } )

      adiv (attr { ClassName "settings" } )
        (alist {
          viewTabs settingsTabs settingsTab
          match! settingsTab with
          | Skills ->
              div [ ClassName "skills" ]
                [ ul []
                    (Skills.all |> List.map viewSkill)
                  checkboxText "Ignore Profession Relationships" Profession.ignoreRelationships ]
          | Crops ->
              div [ ClassName "crops" ]
                [ viewTabs cropTabs cropTab ]
          | Fertilizers ->
              div [ ClassName "fertilizers" ]
                [ priceTable
                    "Fertilizer"
                    Image.fertilizer
                    Fertilizer.name
                    Fertilizer.bestPrice
                    Fertilizer.bestSources
                    Fertilizer.prices
                    Fertilizers.all
                    Fertilizers.sources
                  div [] []
                  checkboxText "Account for Fertilizer Cost" Fertilizer.accountForCost ]
          | Date ->
              div [ ClassName "date" ]
                [ dateInput "Start Date" DateInput.startDate
                  dateInput "End Date" DateInput.endDate ]
          | Misc -> pseudoNothing
          | Mod ->
              div [ ClassName "quality-products" ]
                [ checkboxText "Quality Products" Mod.qualityProducts
                  ul [] (Processors.qualityProducts |> List.choose (fun processor ->
                    match processor.OutputQuality with
                    | QualityProducts p ->
                        li []
                          [ checkboxWith
                              [ div []
                                  (imageAndName Image.processor processor.Name) ]
                              p ]
                        |> Some
                    | _ -> None)) ]
              div [ ClassName "quality-seedmaker" ]
                [ checkboxWith
                    [ div []
                        [ image Image.processor Processors.seedMaker.Name
                          str "Quality Seedmaker" ] ]
                    Mod.qualitySeedMaker
                  ul [] (Mod.qualitySeedMakerAmounts |> Map.toList |> List.map (fun (quality, amount) ->
                    li []
                      [ str <| string quality
                        ainput (attr {
                          typeNumber
                          Min 0
                          !&Value amount
                          OnChange <| setInt amount positive } ) ] )) ]
        } ) ]


// colWidths [ 0.25; 0.15; 0.1; 0.5 ]
// thead []
//   [ tr []
//       [ let fertSort = setSortDelay fertilizerSort fertilizerSortAscending
//         headerSort fertSort FertName "Fertilizer"
//         headerSort fertSort Price "Price"
//         headerSort fertSort Speed "Speed"
//         headerSort fertSort Quality "Quality" ] ]
// atbody noAttr
//   (AVal.map2 (fun sort ascending ->
//       Fertilizers.all |>
//       match sort with
//       | FertName -> fun list -> if ascending then list else list |> List.sortByDescending Fertilizer.name
//       | Quality -> listSortMode ascending Fertilizer.quality
//       | Speed -> listSortMode ascending Fertilizer.speed
//       | Price -> listSortMode ascending (Fertilizer.bestPrice >> (!~)))
//       //AVal.force the best price, since it cannot change from this tab,
//       //and the react element is recreated when coming back to this tab, thereby updating it if it changed
//       fertilizerSort
//       fertilizerSortAscending
//     |> AList.ofAVal
//     |> AList.map viewFert)

let urlUpdate _ =
  let hash = window.location.hash.[1..]
  match Page.parse hash with
  | Some newPage -> setValue page newPage
  | None -> ()

[<EntryPoint>]
let main _ =
  document.addEventListener("readystatechange", fun _ ->
    if document.readyState = "complete" then
      window.addEventListener("hashchange", urlUpdate)
      window.addEventListener("popstate", urlUpdate)
      urlUpdate()
      let app = document.createElement "div"
      document.body.appendChild app |> ignore
      ReactDom.render(ui, app))
  0
