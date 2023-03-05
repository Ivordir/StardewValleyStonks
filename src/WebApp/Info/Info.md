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
  (e.g., both allow all crops to be planted out of season and prohibit Giant Crops from forming).
  The only difference is that Wild Seeds many destroy fertilizer on Ginger Island,
  while they do not in the Greenhouse or on the Farm.

Skill Level
: this is your in-game farming or foraging skill level. Note that you start the game with a skill level of zero.

Skill Buff
: this is the in-game farming or foraging buff you have from food at the time of harvesting.
  See the [farming](https://stardewvalleywiki.com/Farming#Food) and [foraging](https://stardewvalleywiki.com/Foraging#Food)
  pages on the Stardew Valley Wiki for the buff values for various food.

Ignore Skill Level Unlocks
: professions, processors like the Seed Maker, and crafting recipes for Wild Seeds are all locked behind certain skill levels.
  Enabling this setting ensures that these items are always available/active regardless of skill level.

Ignore Profession Conflicts
: level 10 professions are mutually exclusive with one another. Additionally, level 10 professions require the corresponding level 5 profession to be taken.
  Enabling this setting disables the automatic toggling of required professions or untoggling of exclusive professions.

Joja Membership
: if enabled, this lowers most seed prices at JojaMart to match Pierre's.

Bear's Knowledge
: increases the sell price of Blackberries by a factor of 3. You can check your in-game wallet to see if you have this unlocked.

Apply Tiller to Foraged Grapes and Blackberries
: the Tiller profession applies to Grapes grown from Grape Starter and Blackberries harvested from bushes,
  but it does not apply to Grapes and Blackberries harvested from Wild Seeds.
  However, combining a stack of foraged Grapes or Blackberries with another item stack of its Tiller-benefitting counterpart
  causes all items in the new stack to benefit from the Tiller profession.
  Enabling this setting mimics this behaviour and will apply the Tiller profession to Grapes and Blackberries harvested from Wild Seeds.

Profit Margin
: this corresponds to the Profit Margin setting found under advanced game options when creating a new Stardew Valley game.

Seed Strategy
: the "Buy First" strategy mandates that the first seed for a crop must be bought.
  Subsequent plantings of the same crop may continue to buy seeds or use alternative seed sources like the Seed Maker (if it is available/unlocked).
  However, due to a limitation with the Optimizer, choosing this strategy will currently cause the Optimizer to **only** buy seeds.
  With the "Stockpile Seeds" strategy, the first seed is considered free.
  Subsequent seeds may be bought or can come from alternative seed sources.
  The major difference is that an extra seed must be left over to be "stockpiled" for the next year,
  thereby achieving net zero seeds.
  Under the "Ignore" strategy, all harvested items are sold, no seeds are bought, and no seeds are obtained from alternative sources.

Pay for Fertilizer
: if this setting is enabled, the gold cost of buying fertilizer is accounted for.

Pay for Destroyed Fertilizer
: Wild Seeds grown on Ginger Island and Giant Crops grown on the Farm may destroy the fertilizer they were planted on once fully grown.
  Enabling this setting accounts for the cost of replacing the destroyed fertilizer between harvests.

Irrigated
: enable this setting to apply the irrigation growth speed bonus (25%) to Rice and Taro.

Average Possible Giant Crops Per Tile
: todo

Shaving Enchantment
: todo

Special Charm
: you can check your in-game wallet to see if you have this unlocked.
  This setting has little effect but is included nevertheless.

Luck Buff
: this is the in-game luck buff you have at the time of harvesting.
  This setting has little effect but is included nevertheless.

Quality Artisan Products
: if you play with the quality artisan products mod, you can enable this setting.
  Items placed in kegs, etc., will retain their quality once processed into an artisan product.

View with Quality
: this setting changes the prices shown in the products table in the crops tab.
  It only effects the prices in the "Raw" column unless the "quality artisan products" setting is enabled.

Show Normalized Prices
: the prices displayed in the products table are unit sell prices with this setting disabled.
  Enabling this setting instead displays the sell price per input item.

# Ranker
todo

# Optimizer
This section is for nerds and details how the Optimizer works.

# Limitations

The Ranker and Optimizer have several limitations that you may want to be aware of.
In rough order of importance, these are:

- The Optimizer assumes the limiting factor is energy or farm tiles and **not gold**.
  It is for this reason and others that the Optimizer currently cannot maximize ROI (return on investment).
  The Ranker can provide the ROI for **individual** crops,
  but not across multiple crops like the Optmizer does for gold or xp.

  > For example, at the start of the game, you have little gold but comparatively lots of energy.
  > I'm not a speedrunner, but the goal in this case could be to prioritize ROI and scale up as fast as possible,
  > so that you are able to plant enough high-profiting crops to meet your energy capacity.

- When the Seed Strategy is set to "Buy First", the Optimizer will **only** buy seeds
  and will never use other sources like the Seed Maker or crafted Wild Seeds.
  The Ranker does not have this limitation.

- All gold, ROI, and xp values from the Ranker and Optimizer are [expected values](https://en.wikipedia.org/wiki/Expected_value) (i.e., averages).

- The Ranker and Optimizer do not factor in the change of variables over time (e.g., farming level).
  Both the Ranker and Optimizer will be more accurate if used over a shorter time period,
  or if variables do not change over time (e.g., you have the maximum farming level).
  Then again, you can manually change the variable in question and check whether
  a certain crop or output from the Optimizer keeps getting recommended even as the variable changes.

- The Ranker and Optimizer consider raw numbers (gold, xp, etc.), but not other logistical factors like:
  - Processing Time and Capacity
    - Harvesting and replanting is assummed to happen all on the same day, even when using the Seed Maker.

    > For example, cranberries can be more profitable than pumpkins if turned into wine.
    > However, each harvested crop gives (about) 2 cranberries and the regrow time is 5 days -- less than
    > the processing time for wine.
    > You can process the excess cranberries over winter if you do not need immediate cash,
    > or alternatively, you can plant pumpkins.
    > This will give less gold, but won't overload your keg capacity.

  - Time Investment

    > For example, hops is often highly ranked when the keg is available,
    > but are you willing to sacrafice time every morning to harvest all those hops and refill all your kegs?
    > Sometimes yes, but sometimes Starfruit might be "better"
    > even if it makes a little less gold and has a higher upfront investment.
    > Food for thought...

  - Restrictions on When/Where Items Can Be Bought
    - Certain crops and fertilizers are only available from year 2 and onwards. Hint: the "Year 1" preset helps with this.
    - Deluxe Speed-Gro can only be bought on Thursday from the Oasis (if that matters).
    - Strawberry Seeds can only be bought at the Egg Festival.

    > For example, the Sweet Gem Berry is often highly ranked,
    > but it is hard to get any significant amount of Rare Seeds.

- The Ranker and Optimizer operate on a fixed start and end date with a maximum time span of 1 year.
  So, for the Greenhouse and Ginger Island, the first harvest and seed cost for regrow crops is included in its ranking/calculations.
  Over a longer time period, these time and gold investments would be less significant and may result in a higher ranking.

- If a multi-season crop has its growth period split by the chosen start and end date,
  then the Ranker chooses the period with the most harvests (or the least days if there is a tie).

- The Ranker and Optimizer use expected values for:
  - the amount of seeds obtained from the Seed Maker. It is assumed that about 0.51 items are needed to create 1 seed.
  - the harvested amounts of each item from Wild Seeds.
    It is assumed equal amounts of each item are harvested such that the maximum possible amount of Wild Seeds can be crafted.

- The Ranker and Optimizer currently do not factor in leaving fully grown Giant Crops intact to have them potentially become giant.

- Chaining products/processors is currently not supported.

  > For example, Sunflowers can be put into the Seed Maker to make multiple Sunflower Seeds.
  > These seeds, in turn, can then be put into the Oil Maker to make Oil.

- Cross-crop products/interactions are currently not supported.

  > For example, Summer Forage provides Grapes which can then be put into the Seed Maker to make Grape Starter.
  > When Fall comes, these seeds can then be planted.

- The Ranker and Optimizer make other reasonable assumptions:
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
