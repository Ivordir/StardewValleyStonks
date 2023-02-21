namespace StardewValleyStonks.WebApp

open Fable.Core.JsInterop

module Values =
  let private root = Browser.Dom.document.documentElement
  let private setVar (name: string) (value: string) = root?style?setProperty($"--{name}", value)
  let private setPx (name: string) px = setVar name $"{ceil px}px"

  let private fontPx =
    let fontSize: string = Browser.Dom.window?getComputedStyle(root)?getPropertyValue("font-size")
    (fontSize.Substring(0, fontSize.Length - 2)) |> float |> ceil

  let fontSize = int fontPx

  do
    setPx "font-size" fontPx
    setPx "eighth-border" (fontPx / 8.0)
    setPx "sixth-border" (fontPx / 6.0)


[<RequireQualifiedAccess>]
module Class =
  let [<Literal>] open' = "open"

  let [<Literal>] settingsGroup = "settings-group"

  let [<Literal>] icon = "icon"
  let [<Literal>] iconText = "icon-text"
  let [<Literal>] iconProcessor = "icon-processor"

  let [<Literal>] fileInput = "file-input"
  let [<Literal>] fileDropzone = "file-dropzone"
  let [<Literal>] inputBox = "input-box"
  let [<Literal>] checkbox = "checkbox"
  let [<Literal>] label = "label"

  let [<Literal>] disabled = "disabled"

  let [<Literal>] cropQualities = "crop-qualities"
  let [<Literal>] cropQualitiesBars = "crop-qualities-bars"
  let [<Literal>] cropQualitiesProbs = "crop-qualities-probs"

  let [<Literal>] skill = "skill"
  let [<Literal>] skillLevel = "skill-level"
  let [<Literal>] professions = "professions"
  let [<Literal>] profession = "profession"

  let [<Literal>] calendar = "calendar"
  let [<Literal>] calendarSeason = "calendar-season"
  let [<Literal>] calendarHeader = "calendar-header"
  let [<Literal>] calendarDays = "calendar-days"

  let [<Literal>] quality = "quality"
  let [<Literal>] seasons = "seasons"
  let [<Literal>] date = "date"

  let [<Literal>] graph = "graph"
  let [<Literal>] graphControls = "graph-controls"
  let [<Literal>] pairImage = "pair-image"
  let [<Literal>] pairSelect = "pair-select"
  let [<Literal>] summary = "summary"
  let [<Literal>] summaryControls = "summary-controls"

  let [<Literal>] columnSort = "colunmSort"
  let [<Literal>] collapseArrow = "collapse-arrow"
  let [<Literal>] summaryTable = "summary-table"
  let [<Literal>] inputItems = "input-items"

  let [<Literal>] select = "select"
  let [<Literal>] selectControl = "select-control"
  let [<Literal>] selectList = "select-list"
  let [<Literal>] selectEmpty = "select-empty"
