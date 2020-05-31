using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Extensions.Configuration;
using ExtentionsLibrary.Collections;

namespace StardewValleyStonks
{
    public class Data
    {
        public IEnumerable<CropDIO> Crops
        => ShowOutOFSeasonCrops
            ? _Crops
            : _Crops.Where(c => c.IsInSeason);
        public FertilizerDIO[] Fertilizers { get; }
        public Processor[] SellSources { get; }
        public Processor[] QualityProducts { get; }
        public Source[] BuySources { get; }
        public MatchSource[] MatchSources { get; }
        public Processor[] ReplantMethods { get; }
        public bool ShowOutOFSeasonCrops { get; set; }

        private readonly double[] ForageDistribution;
        //private readonly Func<int, RefValue[], IValue[]> ForageAmounts =
        //    new Func<int, RefValue[], IValue[]>((NumChoices, ForageDistribution) =>
        //{
        //    if (NumChoices == 1)
        //    {
        //        return ForageDistribution;
        //    }
        //    IValue[] amounts = new Term[NumChoices];
        //    RefValue numChoices = new RefValue(1.0 / NumChoices);
        //    for (int i = 0; i < ForageDistribution.Length; i++)
        //    {
        //        amounts[i] = new Term(new IValue[]
        //        {
        //            ForageDistribution[i],
        //            numChoices
        //        });
        //    }
        //    return amounts;
        //}).Memoize();

        //private void SetForageAmounts()
        //{
        //    if (Skills.Botanist.Active)
        //    {
        //        ForageDistribution[3]._Value = 1;
        //        ForageDistribution[2]._Value = 0;
        //        ForageDistribution[1]._Value = 0;
        //        ForageDistribution[0]._Value = 0;
        //    }
        //    else
        //    {
        //        ForageDistribution[2]._Value = Skills.Foraging.BuffedLevel / 30.0;
        //        ForageDistribution[1]._Value = Skills.Foraging.BuffedLevel / 15.0 * (1 - ForageDistribution[2]._Value);
        //        ForageDistribution[0]._Value = 1 - ForageDistribution[1]._Value - ForageDistribution[2]._Value;
        //    }
        //    if (Skills.Gatherer.Active)
        //    {
        //        foreach (RefValue value in ForageDistribution)
        //        {
        //            value._Value *= Skills.Gatherer.Value;
        //        }
        //    }
        //}

        readonly CropDIO[] _Crops;
        readonly Date Date;

        public Data(Skills skills, Settings settings, Date date, IConfiguration config)
        {
            Date = date;
            //ForageDistribution = new RefValue[4];
            //for (int i = 0; i < ForageDistribution.Length; i++)
            //{
            //    ForageDistribution[i] = new RefValue(0);
            //}

            //if (true) //forage crop
            //{
            //    int numChoices = 4; //cropItems.Length
            //    IValue[] values = ForageAmounts(numChoices, ForageDistribution);
            //}

            QualityProducts = new Processor[]
            {
                new Processor("Preserves Jar", new ICondition[]{new SkillLvlCondition(skills.Farming, 4)} ),
                new Processor("Keg", new ICondition[]{new SkillLvlCondition(skills.Farming, 8)} ),
                new Processor("Oil Maker", new ICondition[]{new SkillLvlCondition(skills.Farming, 8)} ),
                new Processor("Mill")
                //custom processors affected by Quality Products Mod
            };

            SellSources = new Processor[]
            {
                new Processor("Raw Crop", null, true),
                QualityProducts[0],
                QualityProducts[1],
                QualityProducts[2],
                QualityProducts[3],
                new Processor("Seed Maker", new ICondition[]{ new SkillLvlCondition(skills.Farming, 9) }),
                //custom sellSources
            };

            MatchSources = new MatchSource[]
            {
                new MatchSource("Joja")
            };

            BuySources = new Source[]
            {
                new Source("Pierre"),
                MatchSources[0],
                new Source("Oasis"),
                new Source("Traveling Merchant"),
                new Source("Crafting")
            };

            ReplantMethods = new Processor[]
            {
                new Processor("Buy Seeds"),
                new Processor("Seed Maker", new ICondition[] { new SkillLvlCondition(skills.Farming, 9) }),
            new Processor("Replant Crop")
            };

            Multiplier irrigated = new Multiplier("Irrigated", 0.25);
            Dictionary<string, IMultiplier> multipliers = new Dictionary<string, IMultiplier>
            {
                { skills.Tiller.Name, skills.Tiller },
                { skills.Agriculturist.Name, skills.Agriculturist },
                { skills.Artisan.Name, skills.Artisan },
                { skills.Gatherer.Name, skills.Gatherer },
                { irrigated.Name, irrigated }
            };
            Dictionary<string, Processor> sellSources = SellSources.ToDictionary(s => s.Name);
            Dictionary<string, Source> buySources = BuySources.ToDictionary(s => s.Name);
            Dictionary<string, Processor> replantMethods = ReplantMethods.ToDictionary(s => s.Name);
            Dictionary<string, Skill> skillDict = skills.ToDictionary(s => s.Name);
            Dictionary<string, Item> products = (
                from product in config.GetSection("Products").GetChildren()
                select ParseItem(product, multipliers)).
                ToDictionary(p => p.Name);

            IMultiplier[] agri = new IMultiplier[] { skills.Agriculturist };

            List<CropDIO> crops = new List<CropDIO>();
            foreach (var crop in config.GetSection("Crops").GetChildren())
            {
                string name = crop.GetValue<string>("Name");
                int regrow = crop.GetValue("Regrow", -1);
                var growthMultipliers = crop.GetSection("Growth Multipliers");

                HashSet<string> flags = new HashSet<string>(crop.GetSection("Generate").RawValues());
                Dictionary<Item, List<Process>> processes = new Dictionary<Item, List<Process>>();
                Dictionary<Item, List<Process>> replants = new Dictionary<Item, List<Process>>();

                Item seed = null;
                var cropSection = crop.GetSection("Crop Item");
                Item cropItem = cropSection.Exists() ? ParseItem(cropSection, multipliers)
                    : !flags.Contains("Forage") ? new Item(name, crop.GetValue<int>("Sell"), skills.Tiller)
                    : null;
                if (cropItem != null)
                {
                    processes.Add(cropItem, new List<Process>());
                    replants.Add(cropItem, new List<Process>());
                }
                if (cropSection.GetValue("Seed", false))
                {
                    seed = cropItem;
                    replants[cropItem].Add(new Process(seed, replantMethods["Replant Crop"]));
                }

                Dictionary<Item, double[]> harvestedItems = new Dictionary<Item, double[]>();
                var itemAmounts = crop.GetSection("Harvested Items");
                if (itemAmounts.Exists())
                {
                    foreach (var itemAmount in itemAmounts.GetChildren())
                    {
                        Item item = ParseItem(itemAmount, multipliers);
                        harvestedItems.Add(
                            item,
                            new double[] { itemAmount.GetValue("Amount", 1) });
                        processes.Add(item, new List<Process>());
                        replants.Add(item, new List<Process>());
                        if (itemAmount.GetValue("Seed", false))
                        {
                            seed = item;
                            replants[item].Add(new Process(seed, replantMethods["Replant Crop"]));
                        }
                    }
                }
                if (seed == null)
                {
                    var seedSection = crop.GetSection("Seed");
                    seed = seedSection.Exists()
                        ? ParseItem(seedSection, multipliers)
                        : new Item(name + " Seeds", crop.GetValue<int>("Seed Sell"));
                }

                if (flags.Contains("Vegetable") || flags.Contains("Juice"))
                {
                    processes[cropItem].Add(new Process(
                        cropItem,
                        sellSources["Keg"],
                        new Item(
                            cropItem.Name + " Juice",
                            (int)(cropItem.Price * 2.25),
                            multipliers["Artisan"])));
                }
                else if (flags.Contains("Fruit") || flags.Contains("Wine"))
                {
                    processes[cropItem].Add(new Process(
                        cropItem,
                        sellSources["Keg"],
                        new Item(
                            cropItem.Name + " Wine",
                            cropItem.Price * 3,
                            multipliers["Artisan"])));
                }
                if (flags.Contains("Vegetable") || flags.Contains("Pickle"))
                {
                    processes[cropItem].Add(new Process(
                        cropItem,
                        sellSources["Preserves Jar"],
                        new Item(
                            "Pickeled " + cropItem.Name,
                            cropItem.Price * 2 + 50,
                            multipliers["Artisan"])));
                }
                else if (flags.Contains("Fruit") || flags.Contains("Jam"))
                {
                    processes[cropItem].Add(new Process(
                        cropItem,
                        sellSources["Preserves Jar"],
                        new Item(
                            cropItem.Name + " Jam",
                            cropItem.Price * 2 + 50,
                            multipliers["Artisan"])));
                }
                if (flags.Contains("Seed Maker"))
                {
                    double[] seeds = name == "Ancient Fruit" ? settings.AncientFruitSeeds : settings.Seeds;
                    processes[cropItem].Add(new QualityProcess(
                        cropItem,
                        1,
                        sellSources["Seed Maker"],
                        seed,
                        seeds));
                    replants[cropItem].Add(new QualityProcess(
                        cropItem,
                        1,
                        replantMethods["Seed Maker"],
                        seed,
                        seeds));
                }
                var processData = crop.GetSection("Processes");
                if (processData.Exists())
                {
                    Dictionary<string, Item> harvestedItemWith = harvestedItems.ToDictionary(i => i.Key.Name, i => i.Key);
                    foreach (var process in processData.GetChildren())
                    {
                        var inputSection = process.GetSection("Input Name");
                        Item input = inputSection.Exists()
                            ? harvestedItemWith[inputSection.Value]
                            : cropItem;
                        int inputAmount = process.GetValue("Input Amount", 1);

                        var outputSection = process.GetSection("Output");
                        Item output = outputSection.Exists()
                            ? ParseItem(outputSection, multipliers)
                            : products[process.GetValue<string>("Output Name")];
                        double outputAmount = process.GetValue("Output Amount", 1.0);

                        if (inputAmount != 1 || outputAmount != 1)
                        {
                            processes[cropItem].Add(new RatioProcess(
                                input,
                                inputAmount,
                                sellSources[process.GetValue<string>("Processor")],
                                output,
                                outputAmount,
                                ParseConditions(process.GetSection("Conditions"), skillDict, date)));
                        }
                        else
                        {
                            processes[cropItem].Add(new Process(
                                input,
                                sellSources[process.GetValue<string>("Processor")],
                                output,
                                ParseConditions(process.GetSection("Conditions"), skillDict, date)));
                        }
                    }
                }

                Dictionary<Source, Price> priceFrom = new Dictionary<Source, Price>();
                if (flags.Contains("Pierre"))
                {
                    priceFrom.Add(buySources["Pierre"], new Price(
                        seed.Price * 2,
                        buySources["Pierre"]));
                }
                else if (flags.Contains("Oasis"))
                {
                    priceFrom.Add(buySources["Oasis"], new Price(
                        seed.Price * 2,
                        buySources["Oasis"]));
                }
                priceFrom.AddAll(ParsePrices(crop.GetSection("Seed Prices"), buySources, skillDict, date));
                if (flags.Contains("Joja"))
                {
                    priceFrom.Add(buySources["Joja"], new MatchPrice(
                        (int)(priceFrom[buySources["Pierre"]].Value * 1.25),
                        (MatchSource)buySources["Joja"],
                        priceFrom[buySources["Pierre"]]));
                }

                crops.Add(new CropDIO(
                    name,
                    Enum.Parse<Seasons>(crop.GetValue<string>("Seasons")),
                    regrow == -1
                        ? new Grow(crop.GetSection("Growth Stages").ToArray())
                        : new Regrow(crop.GetSection("Growth Stages").ToArray(), regrow),
                    growthMultipliers.Exists()
                        ? (from multiplier in growthMultipliers.GetChildren()
                            select multipliers[multiplier.Value]).
                            ToArray()
                        : agri,
                    cropItem,
                    crop.GetValue("Extra Crop Chance", 0.0),
                    crop.GetValue("Crop Yield", 1),
                    crop.GetValue("Giant Crop", false),
                    crop.GetValue("Double Crop Chance", true),
                    settings,
                    date,
                    processes.ToDictionary
                    (kvp => kvp.Key, kvp => kvp.Value.ToArray()),
                    replants.ToDictionary
                    (kvp => kvp.Key, kvp => kvp.Value.ToArray()),
                    priceFrom,
                    harvestedItems,
                    seed));
            }
            _Crops = crops.ToArray();

            Fertilizers = (
                from fert in config.GetSection("Fertilizers").GetChildren()
                select new FertilizerDIO(
                    fert.GetValue<string>("Name"),
                    fert.GetValue("Quality", 0),
                    fert.GetValue("Speed", 0.0),
                    ParsePrices(
                        fert.GetSection("Prices"),
                        buySources,
                        skillDict,
                        date))).
                ToArray();
        }

        private static ICondition[] ParseConditions(
            IConfigurationSection conditions,
            Dictionary<string, Skill> skillDict,
            Date date)
        => conditions.Exists()
            ? (from condition in conditions.GetChildren()
               let skill = condition.GetSection("Skill")
               select (skill.Exists() 
               ? (ICondition) new SkillLvlCondition(
                   skillDict[skill.Value],
                   condition.GetValue<int>("Level"))
               : new YearCondition(
                   date,
                   condition.GetValue<int>("Year")))).
              ToArray()
            : null;

        private static Item ParseItem(
            IConfigurationSection item,
            Dictionary<string, IMultiplier> multiplierDict)
        => new Item(
            item.GetValue<string>("Name"),
            item.GetValue<int>("Sell"),
            multiplierDict.GetOrDefault(item.GetValue("Multiplier", string.Empty)));

        private static Dictionary<Source, Price> ParsePrices(
            IConfigurationSection prices,
            Dictionary<string, Source> sources,
            Dictionary<string, Skill> skillDict,
            Date date)
        => prices.Exists()
            ? (from price in prices.GetChildren()
               select new Price(
                  price.GetValue<int>("Buy"),
                  sources[price.GetValue<string>("Name")],
                  ParseConditions(price.GetSection("Conditions"), skillDict, date))).
               ToDictionary(price => price.Source)
            : new Dictionary<Source, Price>();
    }
}
