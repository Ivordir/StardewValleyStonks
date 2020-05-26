using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Configuration.Binder;
using ExtentionsLibrary.Memoization;
using System;
using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Data
    {
        public CropDIO[] Crops { get; }
        public FertilizerDIO[] Fertilizers { get; }
        public Source[] SellSources { get; }
        public Processor[] QualityProducts { get; }
        public Source[] BuySources { get; }
        public Source[] ReplantMethods { get; }

        private readonly Skills Skills;
        private readonly RefValue[] ForageDistribution;
        private readonly Func<int, RefValue[], IValue[]> ForageAmounts =
            new Func<int, RefValue[], IValue[]>((NumChoices, ForageDistribution) =>
        {
            if (NumChoices == 1)
            {
                return ForageDistribution;
            }
            IValue[] amounts = new Term[NumChoices];
            RefValue numChoices = new RefValue(1.0 / NumChoices);
            for(int i = 0; i < ForageDistribution.Length; i++)
            {
                amounts[i] = new Term(new IValue[]
                {
                    ForageDistribution[i],
                    numChoices
                });
            }
            return amounts;
        }).Memoize();

        private void SetForageAmounts()
        {
            if (Skills.Botanist.Active)
            {
                ForageDistribution[3]._Value = 1;
                ForageDistribution[2]._Value = 0;
                ForageDistribution[1]._Value = 0;
                ForageDistribution[0]._Value = 0;
            }
            else
            {
                ForageDistribution[2]._Value = Skills.Foraging.BuffedLevel / 30.0;
                ForageDistribution[1]._Value = Skills.Foraging.BuffedLevel / 15.0 * (1 - ForageDistribution[2]._Value);
                ForageDistribution[0]._Value = 1 - ForageDistribution[1]._Value - ForageDistribution[2]._Value;
            }
            if (Skills.Gatherer.Active)
            {
                foreach(RefValue value in ForageDistribution)
                {
                    value._Value *= Skills.Gatherer.Value;
                }
            }
        }

        public Data(Skills skills, Settings settings, Date date, IConfiguration config)
        {
            Skills = skills;

            ForageDistribution = new RefValue[4];
            for(int i = 0; i < ForageDistribution.Length; i++)
            {
                ForageDistribution[i] = new RefValue(0);
            }

            if(true) //forage crop
            {
                int numChoices = 4; //cropItems.Length
                IValue[] values = ForageAmounts(numChoices, ForageDistribution);
            }

            QualityProducts = new Processor[4] {
                new Processor("Preserves Jar", new ICondition[]{ new SkillLvlCondition(Skills.Farming, 4)}),
                new Processor("Keg", new ICondition[]{ new SkillLvlCondition(Skills.Farming, 8)}),
                new Processor("Oil Maker", new ICondition[]{ new SkillLvlCondition(Skills.Farming, 8)}),
                new Processor("Mill")
                //custom processors affected by Quality Products Mod
            };

            SellSources = new Processor[6]
            {
                new Processor("Raw Crop", null, true),
                QualityProducts[0],
                QualityProducts[1],
                QualityProducts[2],
                QualityProducts[3],
                new Processor("Seeds", null, true)
                //custom sellSources
            };

            BuySources = new Source[5] {
                new Source("Pierre"),
                new Source("Jojo"),
                new Source("Oasis"),
                new Source("Traveling Merchant"),
                new Source("Crafting")
            };

            ReplantMethods = new Source[3] {
                new Source("Buy Seeds"),
                new Source("Seed Maker", new ICondition[]{ new SkillLvlCondition(Skills.Farming, 9)}),
                new Source("Replant Crop")
            };

            Multiplier irrigated = new Multiplier("Irrigated", 0.1);
            Dictionary<string, IMultiplier> multipliers = new Dictionary<string, IMultiplier>
            {
                { Skills.Tiller.Name, Skills.Tiller },
                { Skills.Agriculturist.Name, Skills.Agriculturist },
                { Skills.Artisan.Name, Skills.Artisan },
                { Skills.Gatherer.Name, Skills.Gatherer },
                { irrigated.Name, irrigated }
            };
            Dictionary<string, Source> sellSources = SellSources.ToDictionary(s => s.Name);
            Dictionary<string, Source> buySources = BuySources.ToDictionary(s => s.Name);
            Dictionary<string, Source> replantMethods = ReplantMethods.ToDictionary(s => s.Name);
            Dictionary<string, Skill> skillDict = Skills.ToDictionary(s => s.Name);

            List<CropDIO> crops = new List<CropDIO>();
            //foreach(IConfigurationSection crop in config.GetSection("Crops").GetChildren())
            //{
            //    Grow grow;
            //    IConfigurationSection regrow = crop.GetSection("Regrow");
            //    if(regrow.Exists())
            //    {
            //        grow = new Regrow(
            //            crop.GetValue<int[]>("GrowthStages"),
            //            regrow.Get<int>());
            //    }
            //    else
            //    {
            //        grow = new Grow(crop.GetValue<int[]>("GrowthStages"));
            //    }



            //    crops.Add(new CropDIO(
            //        crop.GetValue<string>("Name"),
            //        Enum.Parse<Seasons>(crop.GetValue<string>("Seasons")),
            //        grow,
            //        ));
            //}
            Crops = new CropDIO[0];

            Fertilizers = config.GetSection("Fertilizers").GetChildren()
                .Select(f => new FertilizerDIO(
                    f.GetValue<string>("Name"),
                    f.GetValue("Quality", 0),
                    f.GetValue<double>("Speed", 0),
                    ParsePrices(f, buySources, skillDict, date)))
                .ToArray();
        }

        private static Dictionary<Source, Price> ParsePrices(
            IConfiguration config,
            Dictionary<string, Source> sources,
            Dictionary<string, Skill> skillDict,
            Date date)
        {
            Dictionary<Source, Price> prices = new Dictionary<Source, Price>();
            foreach (IConfigurationSection price in config.GetSection("Sources").GetChildren())
            {
                Source source = sources[price.GetValue<string>("Name")];
                prices.Add(source, new BuyPrice(
                    price.GetValue<int>("Price"),
                    source,
                    ParseConditions(price.GetSection("Conditions"), skillDict, date)));
            }
            return prices;
        }
        private static ICondition[] ParseConditions(
            IConfigurationSection config,
            Dictionary<string, Skill> skillDict,
            Date date)
        {
            if (!config.Exists())
            {
                return null;
            }
            List<ICondition> conditions = new List<ICondition>();
            foreach (IConfigurationSection condition in config.GetChildren())
            {
                IConfigurationSection skill = condition.GetSection("Skill");
                if (skill.Exists())
                {
                    conditions.Add(new SkillLvlCondition(
                        skillDict[skill.Value],
                        condition.GetValue<int>("Level")));
                }
                else //should be a year condition
                {
                    conditions.Add(new YearCondition(
                        date,
                        condition.GetValue<int>("Year")));
                }
            }
            return conditions.ToArray();
        }

    }
}
