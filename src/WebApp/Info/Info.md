# Introduction
Stardew Valley Stonks has two main functions:

- The [Ranker] displays a bar chart of the best crops, fertilizers, or pairs thereof, for the chosen ranking metric.
  Hovering or clicking on a bar opens a more detailed view showing how its value was calculated.
- The [Optimizer] recommends a near-optimal plan of fertilizers and crop plantings which maximize net gold or xp.

Both the Ranker and Optimizer update in response to any [settings] you change,
allowing you to explore rankings and recommendations for various situations and combinations of crops and fertilizers.

Keep in mind that Stardew Valley Stonks is a tool to help you make informed decisions, and not a source of ultimate truth.
This is because the Ranker and Optimizer operate under a certain set of [limitations] and assumptions.
Understanding the main limitations can help you judge whether recommendations from the Ranker and Optimizer
are ideal for your specific situation or playstyle.

# Settings

If you are unsure about what certain settings do, notes and explanations for each are included below.

Start and End Date
: Stardew Valley Stonks supports dates spanning multiple years but only to a maximum of four seasons.
  So, if the starting and ending seasons are the same, then the starting day cannot be after the ending day.
  For example, starting on Winter 5 and ending on Fall 28 is allowed, but starting on Winter 5 and ending on Winter 1 is not allowed.

Location
: this is the location where crops will be planted. The Greenhouse and Ginger Island are functionally equivalent
  (e.g., both allow all crops to be planted out of season and prohibit giant crops from forming).
  The only difference is that Wild Seeds many destroy fertilizer on Ginger Island,
  while they do not in the Greenhouse or on the Farm.

Skill Level
: this is your in-game farming or foraging skill level. Note that you start the game with a skill level of zero.

Skill Buff
: this is the in-game farming or foraging buff you have from food at the time of harvesting.
  See the [farming](https://stardewvalleywiki.com/Farming#Food) and [foraging](https://stardewvalleywiki.com/Foraging#Food)
  pages on the Stardew Valley Wiki for the buff values for various food items.

Ignore Skill Level Unlocks
: professions, processors like the Seed Maker, and crafting recipes for Wild Seeds are all locked behind certain skill levels.
  Enabling this setting ensures that these items are always available/active regardless of skill level.

Ignore Profession Conflicts
: level 10 professions are mutually exclusive with one another. Additionally, level 10 professions require the corresponding level 5 profession to be taken.
  Enabling this setting disables the automatic toggling of required professions or untoggling of exclusive professions.

Seed Strategy
: the "buy first" strategy mandates that the first seed for a crop must be bought.
  Subsequent plantings of the same crop may continue to buy seeds or use alternative seed sources like the Seed Maker (if it is available/unlocked).
  However, due to a limitation with the Optimizer, choosing this strategy will currently cause the Optimizer to **only** buy seeds.
  With the "stockpile" strategy, the first seed is considered free.
  Subsequent seeds may be bought or can come from alternative seed sources.
  The major difference is that an extra seed must be left over to be "stockpiled" for the next year,
  thereby achieving net zero seeds.
  Under the "ignore" strategy, all harvested items are sold, no seeds are bought, and no seeds are obtained from alternative sources.

Pay for Fertilizer
: if this setting is enabled, the gold cost of buying fertilizer is accounted for.

Pay for Destroyed Fertilizer
: Wild Seeds grown on Ginger Island and giant crops grown on the Farm may destroy the fertilizer they were planted on once fully grown.
  Enabling this setting accounts for the cost of replacing the destroyed fertilizer between harvests.
  This setting is ignored if the "pay for fertilizer" setting is disabled.

Profit Margin
: this corresponds to the profit margin setting found under advanced game options when creating a new Stardew Valley game.

Bear's Knowledge
: increases the sell price of Blackberries by a factor of 3. You can check your in-game wallet to see if you have this unlocked.

Apply Tiller to Foraged Grapes and Blackberries
: the Tiller profession applies to Grapes grown from Grape Starter and Blackberries harvested from bushes,
  but it does not apply to Grapes and Blackberries harvested from Wild Seeds.
  However, combining a stack of foraged Grapes or Blackberries with another item stack of its tiller-benefitting counterpart
  causes all items in the new stack to benefit from the Tiller profession.
  Enabling this setting mimics this behaviour and will apply the Tiller profession to Grapes and Blackberries harvested from Wild Seeds.

Joja Membership
: if enabled, this lowers most seed prices at JojaMart to match Pierre's.

Average Giant Crop Orientations Per Tile
: this setting represents the number of different possible ways a giant crop can spawn on a tile, averaged across all farm tiles.
  A longer explanation with a list of common values is included at the [end](#giant-crops) of this section.

Shaving Enchantment
: if you have an axe with the shaving enchantment, you can select the corresponding tool level here.
  This affects the number of items harvested from a giant crop.
  Surprisingly, gold and steel axes provide the highest expected shaved items.

Irrigated
: enable this setting to apply the irrigation growth speed bonus (25%) to Rice and Taro.

Special Charm
: you can check your in-game wallet to see if you have this unlocked.
  This setting has a very small effect on the number of crops harvested.

Luck Buff
: this is the in-game luck buff you have at the time of harvesting.
  This setting has a very small effect on the number of crops harvested.

Quality Artisan Products
: if you play with the [quality artisan products](https://www.moddrop.com/stardew-valley/mods/707502-quality-artisan-products) mod, you can enable this setting.
  Items placed in kegs, etc., will retain their quality once processed into an artisan product.

Show Prices for Quality
: this setting changes the prices shown in the products table in the crops tab.
  It only affects the prices in the "raw" column unless the "quality artisan products" setting is enabled.

Show Normalized Prices
: the prices displayed in the products table are unit sell prices with this setting disabled.
  Enabling this setting instead displays the sell price per input item.

## Giant Crops
To help explain the "average giant crop orientations per tile" setting, first consider an infinite field of crops.
For any chosen tile, there are 9 different ways a giant crop can spawn on top of any given tile.

![The 9 possible giant crop orientations.](/public/img/Field%20Layouts/Giant%20Crop%20Orientations.webp)

If the size of the field is set to a finite size, say 5x5 tiles,
then the tiles near the edges have less possible ways for giant crops to spawn on them.
By finding the number of possible spawning orientations for each tile and taking the average,
it turns out that the average number of orientations is 3.24 for our 5x5 field example.

![The number of giant crop orientations for each tile on a 5x5 field.](/public/img/Field%20Layouts/Example%20Orientations%20Per%20Tile.png)

Doing this same process for your crop field can be tedious,
since other obstacles in the field like sprinklers and jumino huts block certain spawning locations as well.
So, included below are precomputed averages for various field layouts.
Making a rough estimate for your field using the values below should be good enough.
Note that the numbers below are all overestimates,
since they do not take into account neighboring tiles becoming blocked after the first giant crop is spawned.
So, you can choose to make a slightly lower estimate to compensate.

- For a plain field:

  | Field Size            | Avg. Orientations |
  |:--------------------- | -----------------:|
  | 10x10                 | 5.76              |
  | 15x15                 | 6.76              |
  | 20x20                 | 7.29              |
  | 25x25                 | 7.62              |
  | Infinite              | 9.00              |
  | 17x17 with jumino hut | 6.52              |

- For a field with iridium sprinklers:

  | Field Layout             | Avg. Orientations |
  |:------------------------ | -----------------:|
  | 2x2 sprinklers           | 2.65              |
  | 3x3 sprinklers           | 3.67              |
  | 4x4 sprinklers           | 4.22              |
  | Infinite                 | 6.00              |
  | 3x3 ring with jumino hut | 2.25              |

  ![A 3x3 ring of iridium sprinklers around a jumino hut.](/public/img/Field%20Layouts/Iridium%20Sprinkler%20Ring%20Around%20Jumino%20Hut.png)

- For a field of iridium sprinklers with pressure nozzles:

  | Field Layout             | Avg. Orientations |
  |:------------------------ | -----------------:|
  | 1x1 sprinkler            | 3.00              |
  | 2x2 sprinklers           | 5.06              |
  | 3x3 sprinklers           | 5.83              |
  | 4x4 sprinklers           | 6.23              |
  | Infinite                 | 7.50              |

- Fields of normal or quality sprinklers have no 3x3 open spaces and so cannot spawn giant crops.

# Ranker
The Ranker supports ranking three different set of items: crops, fertilizers, and "all pairs".

- Ranking crops causes the Ranker to choose the fertilizer for each crop that gives the highest rank value.
  Sometimes, the best fertilizer actually turns out to be no fertilizer. This is common when ranking by ROI.

- Ranking fertilizers, unsurprisingly, does the reverse.
  It will cause the Ranker to choose the best crop for each fertilizer — the crop that gives the highest rank value.

- Ranking "all pairs" shows all possible combinations of crops and fertilizers on the chart.
  You can use the brush at the bottom of the chart to show more or less bars on the chart.

The Ranker can rank the above items by three different metrics: gold, ROI, or xp.

- Ranking by gold uses the net gold gained by the crop/fertilizer over the course of the specified start and end date.
  Fertilizer and seed costs are subtracted as necessary based on the "pay for fertilizer" settings and the
  "seed strategy" setting.

- Ranking by ROI is the same as ranking by gold but divides by the initial investment.
  The initial investment is defined as the cost of the initial fertilizer
  plus the first seed price if the "buy first" seed strategy is used.
  The "stockpile" and "ignore" seed strategies indicate that there is no initial seed investment.
  Similarly, if the "pay for fertilizer" setting is disabled, then the fertilizer does not contribute to the initial investment.
  If the initial investment is zero, then the crop/fertilizer is considered invalid and hidden on the chart unless
  the "show invalid" setting is enabled.

- Ranking by xp utilizes the total farming or foraging xp gained from harvesting.
  Note that invalid entries are still hidden even if the reason for being invalid is monetary in nature.
  For example, if the seed strategy is set to "buy first", then crops without a seed price are still considered invalid
  even though they are being ranked by xp.

The Ranker can normalize the above rank metrics to a per season or per day basis.
Note that there is actually a slight difference between the "per season" and "per day" settings explained below.

- Normalizing by season divides the rank value by the number of **days the crop is in season**.
  It then multiplies this value by 28.

- Normalizing by day divides the rank value by the number of **days needing watering**.

- The "total" setting applies no normalization and heavily favors crops that can grow in multiple seasons.

If you care more about how much value you get from spending energy watering, then "per day" is a better setting for you.
Otherwise, if you care more about how much value you can squeeze into the season, then "per season" is a better setting in this case.

Finally, the "show invalid" setting unhides entries that are considered invalid (e.g., they have no possible harvests or have no seed price).
These entries will appear at the end of the x-axis on the bar chart, and the reason(s) they are considered invalid can be inspected.
This setting can be useful to show you why certain crops are not appearing in the recommendations from the Optimizer.

# Optimizer
For those interested, this section details the optimization problem and how the Optimizer works.

## Problem Description
The core optimization problem can be classified as an unbounded [knapsack problem](https://en.wikipedia.org/wiki/Knapsack_problem).
This is perhaps easy to see when considering a single season and some arbitrary fertilizer.
We have a set of crops and the goal is to find some number of harvests for each crop
which maximizes gold or xp while not exceeding the number of days in the season.

But on top of this, the Optimizer accounts for the following additional constraints and caveats:

- Crops that regrow have two different growth times: the initial growth time for the first harvest
  and the regrow time for subsequent harvests.
  The first harvest introduces an investment/implication constraint where the regrow harvests can only happen if the initial harvest happens.

- Regrow crops must "end" the season, since they remain in the ground until they become out of season.
  As a consequence, there can only be at most one regrow crop per season.

- Giant crops and Wild Seeds may destroy their fertilizer after becoming fully grown.
  Fertilizer should be replaced between harvests (accounting for some cost) in order to
  uphold the growth time and expected profit per harvest (i.e., linearity of the objective function).
  But if it is the last crop of the season, then the fertilizer destroyed by the last harvest does not have to be replaced.
  Of course, this is mutually exclusive with a regrow crop which must also be the last crop of the season.

- Crops can grow in two consecutive seasons.
  If a crop is planted at the end of the season and grows into the next season,
  then the fertilizer it was planted in is not lost, but rather carried over to the next season.
  So, this saves the cost of having to buy the same fertilizer again.
  Additionally, this crop also gains a day of growth that would normally not be utilized
  (from the last day of the previous season to the first day of the next season).

Put together, a well formed solution must plant crops in the following form/order:

- zero or more non-regrow crop harvests in the starting season
- zero or more of the following:
  - a non-regrow, cross-season harvest followed by...
  - zero or more non-regrow harvests in this next season
- at most one of the following:
  - a harvest with unreplaced fertilizer
  - harvests of a regrow crop for the remaining days/season(s) until it goes out of season or the end date is reached, whichever comes first
- repeat all the above starting in the next season if the end date has not been reached

## Solution
Stardew Valley Stonks uses an integer [linear programming](https://en.wikipedia.org/wiki/Linear_programming) solver to find the optimal solution.
This is in contrast to a [dynammic programming](https://en.wikipedia.org/wiki/Dynamic_programming) approach
which could become more and more unwieldy as the number and types of constraints increase — a situation ripe for bugs.
By formulating the problem in terms of a linear programming problem,
the numerous constraints can be described in a more declarative fashion.
To obtain the optimal solution, all we have to do is properly formulate our constraints and variables,
which should be much more straightforward compared to the dynamic programming method.

Currently, the Optimizer handles the cross-season capability of crops detailed above
by solving (in the worst case) a subproblem for each unique combination of fertilizer, starting season, and ending season.
For example, a starting season of Spring and an ending season of Fall has 6 possible combinations:

- Spring – Spring
- Spring – Summer
- Spring – Fall
- Summer – Summer
- Summer – Fall
- Fall – Fall

If all 7 fertilizers are selected (this includes no fertilizer), then this gives 7 x 6 = 42 subproblems that need to be solved.
For each subproblem, the Optimizer constructs a model containing a set of variables and constraints and gives it the solver.
Once all the subproblems have been solved, a [weighted interval scheduling](https://en.wikipedia.org/wiki/Interval_scheduling)
algorithm is used to piece together a sequence of non-overlapping subproblems that provide the maximum value.

> Surprisingly, I was only able to find [one](https://www.npmjs.com/package/javascript-lp-solver) linear programming solver available on NPM that:
>
> - could solve problems with integer variables
> - and was not a WebAssembly port of an existing solver (these tend to have large bundle sizes which is not ideal)
>
> While the solver linked above is wonderful, its performance, however, was lacking for this use case.
> That is, the solver should idealy be able solve all subproblems in under 100ms to provide
> immediate, interactive feedback to the user which is constantly editing settings.
> Because of this, I made [my own](https://github.com/Ivordir/YALPS) linear programming solver based upon the one linked above.
> I targeted any low hanging fruit for performance improvement,
> such that I hope you did not encounter the Optimizer taking a noticeable amount of time.
> There are still plenty of areas available for improvement, particularly with the integer solver,
> but I stopped when a sufficient level of performance was achieved so that I could focus on Stardew Valley Stonks.

# Limitations

The Ranker and Optimizer have several main limitations that you may want to be aware of.
In rough order of importance, these are:

- The Optimizer assumes the limiting factor is energy or farm tiles and **not gold**.
  It is for this reason and others that the Optimizer currently cannot maximize ROI (return on investment).
  The Ranker can provide the ROI for **individual** crops,
  but not across multiple crops like the Optmizer does for gold or xp.

  > For example, at the very start of the game, you have little gold but comparatively lots of energy.
  > I'm not a speedrunner, but the goal in this case could be to prioritize ROI and scale up as fast as possible,
  > so that you are able to plant enough high-profiting crops to meet your energy capacity.
  > In this case, the Optimizer may not be giving the "best" recommendation, but it could still be giving a decent one.

- When the "seed strategy" is set to "buy first", the Optimizer will **only** buy seeds
  and will never use other sources like the Seed Maker or crafted Wild Seeds.
  The Ranker does not have this limitation.

- The Ranker and Optimizer do not factor in the change of variables over time (i.e., farming level).
  Both the Ranker and Optimizer will be more accurate if used over a shorter time period,
  or if variables do not change over time (i.e., you have the maximum farming level).

  > One workaround is to manually check the recommendations for the next few farming levels and see whether the same crops keep getting recommended.

- The Ranker and Optimizer consider raw numbers (gold, xp, etc.), but not other logistical factors like:
  - Processing Time and Capacity
    - Harvesting and replanting is assumed to happen all on the same day, even when using the Seed Maker.

    > For example, cranberries can be more profitable than pumpkins if turned into wine.
    > However, each harvested crop gives (about) 2 cranberries and the regrow time is 5 days -- less than
    > the processing time for wine.
    > You can process the excess cranberries over winter if you do not need immediate cash,
    > or alternatively, you can plant pumpkins.
    > This will give less gold, but won't overload your keg capacity.

  - Time Investment

    > For example, hops is often highly ranked when the keg is available,
    > but are you willing to sacrifice time every morning to harvest all those hops and refill all your kegs?
    > Sometimes yes, but sometimes Starfruit might be "better"
    > even if it makes a little less gold and has a higher upfront investment.
    > Food for thought...

  - Restrictions on When/Where Items Can Be Bought
    - Certain crops and fertilizers are only available from year 2 and onwards. Hint: the "Year 1" preset helps with this.
    - Deluxe Speed-Gro can only be bought on Thursday from the Oasis (if that matters).
    - Strawberry Seeds can only be bought at the Egg Festival.

    > For example, the Sweet Gem Berry is often highly ranked in terms of gold,
    > but it is hard to get any significant amount of Rare Seeds.

- The Ranker and Optimizer operate on a fixed start and end date with a maximum time span of 1 year.
  So, for the Greenhouse and Ginger Island, the first harvest and seed cost for regrow crops is included in its ranking/calculations.
  Over a longer time period, these time and gold investments would be less significant and may result in a higher ranking.

- If a multi-season crop has its growth period split by the chosen start and end date,
  then the Ranker chooses the period with the most harvests (or the least days if there is a tie).

## Additional
Other less important limitations include:

- All gold, ROI, and xp values from the Ranker and Optimizer are [expected values](https://en.wikipedia.org/wiki/Expected_value) (i.e., averages).
  Stardew Valley Stonks makes no effort to track variance or the minimum or maximum possible values.

- The Ranker and Optimizer also use expected values for:
  - the amount of seeds obtained from the Seed Maker. It is assumed that about 0.51 items are needed to create 1 seed.
  - the harvested amounts of each item from Wild Seeds.
    It is assumed that equal amounts of each item are harvested such that the maximum possible amount of Wild Seeds can be crafted.

- The Ranker and Optimizer currently do not factor in leaving fully grown crops intact overnight to have them potentially become giant.

- Chaining products/processors is currently not considered.

  > For example, Sunflowers can be put into the Seed Maker to make multiple Sunflower Seeds.
  > These seeds, in turn, can then be put into the Oil Maker to make Oil.

- Cross-crop products/interactions are currently not considered.

  > For example, Summer Forage provides Grapes which can then be put into the Seed Maker to make Grape Starter.
  > When Fall comes, these seeds can then be planted.

- The Optimizer makes other reasonable assumptions:
  - Regrow crops stay in the ground until they are out of season (or the end date is reached).
    This means there is at most one regrow crop per season.
  - There is at most one fertilizer per season.

# Credits
- The [Stardew Valley Stonks](https://fontstruct.com/fontstructions/show/2216855) font is based off
  [SVThin](https://fontstruct.com/fontstructions/show/1543912) by "Omniros".
  Both are available under the [Open Font License](https://fontstruct.com/fontstructions/license/1543912/svthin).
- Thanks to [Concerned Ape](https://twitter.com/ConcernedApe) for making the original font of course!
  I think he also made a whole game called [Stardew Valley](https://www.stardewvalley.net/) or something.
  But actually, thanks for making an amazing game and supporting its awesome community!
- The [Stardew Valley Wiki](https://www.stardewvalleywiki.com/Stardew_Valley_Wiki) is a fantastic resource, you should check it out!
  Major props to everyone who contributed to make it what it is today.
- [Stardew Valley Profits](https://thorinair.github.io/Stardew-Profits/): the OG profit calculator which gave inspiration for the Ranker.
- The [Stardew Valley Planner](https://stardew.info/planner/) was used for the field layout images in the [giant crops]() section.
- I build off the shoulders of giants, so you can also check out the
  list of [technologies that made this site possible](https://github.com/Ivordir/StardewValleyStonks/#technologies-that-make-this-site-possible)
  if you're interested.

# Contribute
- For bugs, please open an [issue][] or submit a pull request on [Github][].
- For suggestions, you can either make a new [discussion][] or open an [issue][] on Github.
- For questions and other feedback, you can make a new [discussion][] on Github.

[github]: https://github.com/Ivordir/StardewValleyStonks/
[issue]: https://github.com/Ivordir/StardewValleyStonks/issues
[discussion]: https://github.com/Ivordir/StardewValleyStonks/discussions
