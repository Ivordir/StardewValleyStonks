namespace StardewValleyStonks.WebApp.View

open Fable.Core.JsInterop

module Values =
  let private root = Browser.Dom.document.documentElement
  let private setVar (name: string) (value: string) = root?style?setProperty($"--{name}", value)
  let private setPx (name: string) px = setVar name $"{ceil px}px"

  let fontPx =
    let fontSize: string = Browser.Dom.window?getComputedStyle(root)?getPropertyValue("font-size")
    (fontSize.Substring(0, fontSize.Length - 2)) |> float |> ceil

  do
    setPx "border-thin" (max 1.0 (fontPx / 8.0))
    setPx "border-med" (max 1.5 (fontPx / 6.0))


[<RequireQualifiedAccess>]
module Class =
  let [<Literal>] icon = "icon"
  let [<Literal>] iconText = "icon-text"
  let [<Literal>] iconProcessor = "icon-processor"
  let [<Literal>] iconMessage = "icon-message"
  let [<Literal>] quality = "quality"

  let [<Literal>] button = "button"
  let [<Literal>] ok = "ok"
  let [<Literal>] cancel = "cancel"
  let [<Literal>] edit = "edit"
  let [<Literal>] back = "back"

  let [<Literal>] fileInput = "file-input"
  let [<Literal>] inputBox = "input-box"
  let [<Literal>] numberAndRange = "number-and-range"
  let [<Literal>] checkbox = "checkbox"
  let [<Literal>] hiddenText = "hidden-text"
  let [<Literal>] labelText = "label-text"
  let [<Literal>] labelHidden = "label-hidden"

  let [<Literal>] disabled = "disabled"

  let [<Literal>] seasons = "seasons"
  let [<Literal>] date = "date"

  let [<Literal>] settingsColumns = "settings-columns"
  let [<Literal>] settingsColumn = "settings-column"
  let [<Literal>] settingsGroup = "settings-group"
  let [<Literal>] messages = "messages"
  let [<Literal>] messagesLarge = "messages-large"
  let [<Literal>] preset = "preset"

  let [<Literal>] skill = "skill"
  let [<Literal>] skillLevel = "skill-level"
  let [<Literal>] professions = "professions"
  let [<Literal>] cropQualities = "crop-qualities"

  let [<Literal>] calendar = "calendar"
  let [<Literal>] calendarSeason = "calendar-season"
  let [<Literal>] calendarHeader = "calendar-header"
  let [<Literal>] calendarDays = "calendar-days"

  let [<Literal>] columnSelect = "column-select"
  let [<Literal>] columnSort = "column-sort"
  let [<Literal>] collapseArrow = "collapse-arrow"
  let [<Literal>] expanded = "expanded"
  let [<Literal>] collapsed = "collapsed"
  let [<Literal>] inputItems = "input-items"

  let [<Literal>] select = "select"
  let [<Literal>] selectControl = "select-control"
  let [<Literal>] selectList = "select-list"
  let [<Literal>] empty = "empty"
