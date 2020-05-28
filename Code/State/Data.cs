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
        public Source[] ReplantMethods { get; }
        public bool ShowOutOFSeasonCrops { get; set; }

        private readonly RefValue[] ForageDistribution;
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

            Processor SeedMakerForSeeds = new Processor("Seed Maker", new ICondition[] { new SkillLvlCondition(skills.Farming, 9) });
            ReplantMethods = new Source[]
            {
                new Source("Buy Seeds"),
                SeedMakerForSeeds,
                new Source("Replant Crop")
            };


            Multiplier irrigated = new Multiplier("Irrigated", 0.1);
            Dictionary<string, IMultiplier> multiplierDict = new Dictionary<string, IMultiplier>
            {
                { skills.Tiller.Name, skills.Tiller },
                { skills.Agriculturist.Name, skills.Agriculturist },
                { skills.Artisan.Name, skills.Artisan },
                { skills.Gatherer.Name, skills.Gatherer },
                { irrigated.Name, irrigated }
            };
            Dictionary<string, Processor> sellSources = SellSources.ToDictionary(s => s.Name);
            Dictionary<string, Source> buySources = BuySources.ToDictionary(s => s.Name);
            Dictionary<string, Source> replantMethods = ReplantMethods.ToDictionary(s => s.Name);
            Dictionary<string, Skill> skillDict = skills.ToDictionary(s => s.Name);

            IMultiplier[] agri = new IMultiplier[] { skills.Agriculturist };
            _Crops = (
                from crop in config.GetSection("Crops").GetChildren()
                let name = crop.GetValue<string>("Name")
                let regrow = crop.GetValue("Regrow", -1)
                let growthMultipliers = crop.GetSection("Growth Multipliers")

                let cropItemSection = crop.GetSection("Crop Item")
                let cropItem = cropItemSection.Exists()
                    ? ParseItem(cropItemSection, multiplierDict)
                    : new Item(name, crop.GetValue<int>("Price"), skills.Tiller)

                let seedSection = crop.GetSection("Seed")
                let seed = cropItemSection.Exists() && cropItemSection.GetValue("Seed", false) ? cropItem
                    : seedSection.Exists() ? new Item(
                        seedSection.GetValue<string>("Name"),
                        seedSection.GetValue<int>("Sell"),
                        null,
                        1)
                    : new Item(
                        name + " Seeds",
                        crop.GetValue<int>("Seed Sell"),
                        null,
                        1)

                let generate = new HashSet<string>(crop.GetSection("Generate").RawValues())
                select new CropDIO(
                    name,
                    Enum.Parse<Seasons>(crop.GetValue<string>("Seasons")),
                    regrow == -1
                    ? new Grow(crop.GetSection("Growth Stages").ToArray())
                    : new Regrow(crop.GetSection("Growth Stages").ToArray(), regrow),
                    growthMultipliers.Exists()
                    ? (from multiplier in growthMultipliers.GetChildren()
                       select multiplierDict[multiplier.Value]).
                      ToArray()
                    : agri,
                    cropItem,
                    crop.GetValue("Extra Crop Chance", 0.0),
                    crop.GetValue("Crop Yield", 1),
                    crop.GetValue("Giant Crop", false),
                    crop.GetValue("Double Crop Chance", true),
                    settings,
                    date,
                    ParseProcesses(cropItem, generate, sellSources, multiplierDict),
                    //null,
                    ParsePrices(seed, generate, crop.GetSection("Seed Prices"), buySources, skillDict, date))).
                ToArray();

            Fertilizers = (
                from fert in config.GetSection("Fertilizers").GetChildren()
                select new FertilizerDIO(
                    fert.GetValue<string>("Name"),
                    fert.GetValue("Quality", 0),
                    fert.GetValue("Speed", 0.0),
                    ParsePrices(
                        fert.GetSection("Sources"),
                        buySources,
                        skillDict,
                        date))).
                ToArray();
        }

        private static Dictionary<Source, Price> ParsePrices(
            IConfigurationSection prices,
            Dictionary<string, Source> sources,
            Dictionary<string, Skill> skillDict,
            Date date)
        => prices.Exists()
            ? (from price in prices.GetChildren()
               select new Price(
                  price.GetValue<int>("Price"),
                  sources[price.GetValue<string>("Name")],
                  ParseConditions(price.GetSection("Conditions"), skillDict, date))).
                ToDictionary(price => price.Source)
            : new Dictionary<Source, Price>();

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
            item.GetValue<int>("Price"),
            multiplierDict.GetOrDefault(item.GetValue<string>("Multiplier")));
        private static Process[] ParseProcesses(
            Item cropItem,
            HashSet<string> flags,
            Dictionary<string, Processor> sellSources,
            Dictionary<string, IMultiplier> multipliers)
        {
            List<Process> processes = new List<Process>
            {
                new Process(cropItem, sellSources["Raw Crop"])
            };
            if (flags.Contains("Vegetable") || flags.Contains("Juice"))
            {
                processes.Add(new Process(
                    cropItem,
                    sellSources["Keg"],
                    new Item(
                        cropItem.Name + " Juice",
                        (int)(cropItem.Price * 2.25),
                        multipliers["Artisan"])));
            }
            else if (flags.Contains("Fruit") || flags.Contains("Wine"))
            {
                processes.Add(new Process(
                    cropItem,
                    sellSources["Keg"],
                    new Item(
                        cropItem.Name + " Wine",
                        cropItem.Price * 3,
                        multipliers["Artisan"])));
            }
            if (flags.Contains("Vegetable") || flags.Contains("Pickle"))
            {
                processes.Add(new Process(
                    cropItem,
                    sellSources["Preserves Jar"],
                    new Item(
                        "Pickeled " + cropItem.Name,
                        cropItem.Price * 2 + 50,
                        multipliers["Artisan"])));
            }
            else if (flags.Contains("Fruit") || flags.Contains("Jam"))
            {
                processes.Add(new Process(
                    cropItem,
                    sellSources["Preserves Jar"],
                    new Item(
                        cropItem.Name + " Jam",
                        cropItem.Price * 2 + 50,
                        multipliers["Artisan"])));
            }
            if (flags.Contains("Seed Maker"))
            {
                //wip
            }
            return processes.ToArray();
        }

        private static Dictionary<Source, Price> ParsePrices(
            Item seed,
            HashSet<string> flags,
            IConfigurationSection prices,
            Dictionary<string, Source> sources,
            Dictionary<string, Skill> skillDict,
            Date date)
        {
            Dictionary<Source, Price> priceFrom = ParsePrices(prices, sources, skillDict, date);
            if (flags.Contains("Pierre") && flags.Contains("Joja"))
            {
                Price pierre = new Price(seed.Price * 2, sources["Pierre"]);
                priceFrom.Add(sources["Pierre"], pierre);
                priceFrom.Add(
                    sources["Joja"],
                    new MatchPrice(
                        (int)(pierre.Value * 1.25),
                        (MatchSource)sources["Joja"],
                        pierre));
            }
            else if (flags.Contains("Oasis"))
            {
                priceFrom.Add(sources["Oasis"], new Price(seed.Price * 2, sources["Oasis"]));
            }
            return priceFrom;
        }
    }
}
