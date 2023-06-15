# Stardew Valley Stonks ![Github Pages Status](https://github.com/Ivordir/StardewValleyStonks/actions/workflows/main.yaml/badge.svg)

Stardew Valley Stonks is a web app that has two main functions.
- The Ranker displays a bar chart of the best crops, fertilizers, or pairs thereof, for the chosen ranking metric.
  Hovering or clicking on a bar opens a more detailed view showing how its value was calculated.
- The Optimizer uses a linear programming solver to display a near-optimal plan of fertilizers and crop plantings which maximize net gold or xp.

Both the Ranker and Optimizer update in response to changes in various settings set by the user.
This includes the start and end date, skills and professions, or enabling/disabling certain crops, products, or fertilizers.
For more fine grained control, Stardew Valley Stonks also supports adding custom prices for fertilizers, seeds, and sold items.
Finally, the current settings can be stored a preset for later use. You can even import Stardew Valley save games as a preset.

View the live app at: https://ivordir.github.io/StardewValleyStonks/

The [info page](https://ivordir.github.io/StardewValleyStonks/info.html) explains the web app in more detail and lists limitations with the Ranker and Optimizer.

# Technologies that make this site possible
- [Fable](https://github.com/fable-compiler/Fable): the F\# to JS compiler
- [Preact](https://github.com/preactjs/preact): the fast, React-compatible alternative
- [Feliz](https://github.com/Zaid-Ajaj/Feliz): a F\# React API
- [Recharts](https://github.com/recharts/recharts): a React charting library
- [Parcel](https://github.com/parcel-bundler/parcel): the minimal configuration web bundler
- [Pandoc](https://github.com/jgm/pandoc): the universal markup converter
- [YALPS](https://github.com/Ivordir/YALPS): my very own linear programming solver written in typescript
- and many others!

Also, don't forget the [credits](https://ivordir.github.io/StardewValleyStonks/info.html#credits) listed on the info page!

# Running Local Server
## Requirements:
- [dotnet SDK 7](https://dotnet.microsoft.com/download)
- [Node.js](https://nodejs.org/)
- [pnpm](https://pnpm.io/)
- [pandoc](https://pandoc.org/)

## Installation
```
pnpm i
```

## Debugging / Live Server
If you're using vscode, simply go to the `Run and Debug` tab and run either `Debug with Chrome` or `Debug with Firefox`.

Alternatively, run the `Watch` task and go to <http://localhost:1234> in your browser.

If you're not using vscode, this can be done manually by running the following in three separate terminal instances:
```
pnpm watch:pandoc
```
```
pnpm watch:fable
```
```
pnpm watch
```
Then, go to <http://localhost:1234> in your browser.
