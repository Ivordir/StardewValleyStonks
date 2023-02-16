namespace StardewValleyStonks.WebApp

open Fable.Core.JsInterop

module Values =
  let private root = Browser.Dom.document.documentElement
  let private setVar (name: string) (value: string) = root?style?setProperty($"--{name}", value)
  let private setValue (name: string) (value: float) = setVar name (string value)
  let private setPx (name: string) px = setVar name $"{ceil px}px"

  let private fontPx =
    let fontSize: string = Browser.Dom.window?getComputedStyle(root)?getPropertyValue("font-size")
    (fontSize.Substring(0, fontSize.Length - 2)) |> float |> ceil

  let fontSize = int fontPx

  // set icon size to a multiple of 8 pixels,
  // preferring to upsize instead of downsizing
  let iconScale =
    let div = fontPx / 8.0
    let rem = fontPx % 8.0
    let num8 =
      if rem <= 2.0
      then floor div
      else ceil div

    max 1.0 (num8 / 2.0) // scale to 16px

  let iconSize = int (iconScale * 16.0)

  do
    setPx "font-size" fontPx
    setPx "eighth-border" (fontPx / 8.0)
    setPx "sixth-border" (fontPx / 6.0)
    let sp = fontPx / 16.0
    setPx "min-size" (sp * 48.0)
    setValue "icon-scale" iconScale
    setPx "icon-size" iconSize



[<RequireQualifiedAccess>]
module Class =
  let inline private className (name: string) = Feliz.prop.className name

  let iconProcessor = className "icon-processor"
  let iconProcessorLarge = className "icon-processor-large"

  let fileInput = className "file-input"

  let active = className "active"
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
  let seasons = className "seasons"
  let date = className "date"

  let breakdownTable = className "breakdown-table"

  let graph = className "graph"
  let graphControls = className "controls"
  let auditGraph = className "audit-graph"
  let auditGraphSelect = className "audit-graph-select"
