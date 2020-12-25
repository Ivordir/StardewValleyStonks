namespace StardewValleyStonks

open FSharp.Data.Adaptive
open Adaptive
open Util


module Sources =
  let all =
    [ "Pierre"
      "Joja"
      "Oasis"
      "Traveling Merchant"
      "Crafting" ]


type PriceMultiplier =
  { Name: string
    Value: float aval
    MultiplyOn: bool
    Selected: bool cval
    Active: bool aval }

module PriceMultiplier =
  let name multiplier = multiplier.Name
  let value multiplier = multiplier.Value
  let multiplyOn multiplier = multiplier.MultiplyOn
  let selected multiplier = multiplier.Selected

  let validValue: float -> _ = positive

  let toggleSelected = selected >> toggleDelay

  let create name multiplier multiplyOn =
    let selected = cval false
    { Name = name
      Value = !@(validValue multiplier)
      MultiplyOn = multiplyOn
      Selected = selected
      Active = selected .= !@multiplyOn }

module PriceMultipliers =
  let jojaMembership = PriceMultiplier.create "Joja Membership" 1.25 false

  let all = [ jojaMembership ]


type PriceBase =
  { Value: int aval
    Source: string
    SourceOverride: Override cval
    Selected: bool aval }

type Price =
  | BuyPrice of PriceBase
  | RelativePrice of
      {| Multiplier: PriceMultiplier
         RelativeTo: PriceBase
         Price: int aval
         Source: string
         SourceOverride: Override cval
         Selected: bool aval |}

module Price =
  let source = function
    | BuyPrice p -> p.Source
    | RelativePrice r -> r.Source

  let value = function
    | BuyPrice b -> b.Value
    | RelativePrice r -> r.Price

  let sourceOverride = function
    | BuyPrice p -> p.SourceOverride
    | RelativePrice r -> r.SourceOverride
    
  let selected = function
    | BuyPrice b -> b.Selected
    | RelativePrice r -> r.Selected

  let private bestPrice: Price alist -> _ = AList.filterA selected >> AList.mapA value >> AList.tryMin
  let private bestSources bestPrice = AList.filterA (value >> (.=) (aDefaultValue -1 bestPrice)) >> AList.map source
  let bestPriceAndSources list =
    let prices = AList.ofList list
    let bestPrice = bestPrice prices
    bestPrice, bestSources bestPrice prices

  let validPrice: int -> _ = positive

  let private createBase sourceSelected price source =
    let over = cval None
    { Value = price
      Source = source
      SourceOverride = over
      Selected = sourceSelected source |> aSelectedOverride over }

  let createBaseWith sources = validPrice >> (!@) >> createBase sources
  let createWith sources = createBaseWith sources >>| BuyPrice

  let createBaseSeedWith sources = (.*) !@2 >> createBase sources
  let createSeedWith sources =  createBaseSeedWith sources >>| BuyPrice

  let createRelativeWith sourceSelected source multiplier relative =
    let over = cval None
    RelativePrice
      {| Source = source
         Multiplier = multiplier
         RelativeTo = relative
         SourceOverride = over
         Selected = sourceSelected source |> aSelectedOverride over
         Price = aCondApply relative.Value multiplier.Value multiplier.Active |}
