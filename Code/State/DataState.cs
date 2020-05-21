using Microsoft.Extensions.Configuration;
using System;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class DataState
    {
        public Quality[] Qualities { get; }
        public CropDIO[] Crops { get; }
        public FertilizerDIO[] Fertilizers { get; }
        public Source[] SellSources { get; }
        public Processor[] Processors { get; }
        public Source[] BuySources { get; }
        public Source[] ReplantMethods { get; }

        private readonly SkillsState Skills;
        private readonly RefValue QualityCrops;
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
                ForageDistribution[3].Value = 1;
                ForageDistribution[2].Value = 0;
                ForageDistribution[1].Value = 0;
                ForageDistribution[0].Value = 0;
            }
            else
            {
                ForageDistribution[2].Value = Skills.Foraging.BuffedLevel / 30.0;
                ForageDistribution[1].Value = Skills.Foraging.BuffedLevel / 15.0 * (1 - ForageDistribution[2].Value);
                ForageDistribution[0].Value = 1 - ForageDistribution[1].Value - ForageDistribution[2].Value;
            }
            if (Skills.Gatherer.Active)
            {
                foreach(RefValue value in ForageDistribution)
                {
                    value.Value *= Skills.Gatherer.Value;
                }
            }
        }

        public DataState(SkillsState skills, SettingsState settings, DateState date, IConfiguration config)
        {
            Skills = skills;

            ForageDistribution = new RefValue[4];
            for(int i = 0; i < ForageDistribution.Length; i++)
            {
                ForageDistribution[i] = new RefValue(0);
            }

            QualityCrops = new RefValue(1);
            if (true) //cropItems.Length == 1
            {
                IValue qualityCrops = QualityCrops;
                IValue normalCrops;
                if (true) //has doublechance
                {
                    double extra = 0;
                    RefValue e = new RefValue(extra);
                    //normalCrops = P(d) * e + P(d) + e
                    normalCrops = new Expression(new IValue[]
                    {
                    new Term(new IValue[] { settings.DoubleCropProb, e }),
                    settings.DoubleCropProb,
                    e
                    });
                }
                if (true) //is giantcrop
                {
                    //qualityCrops = P(!giantcrop)
                    qualityCrops = settings.NoGiantCropProb;
                    //normalCrops = previousNormalCrops * P(!giantcrop) + GiantCrops
                    normalCrops = new Expression(new IValue[]
                    {
                    new Term(new IValue[]
                    {
                        normalCrops,
                        settings.NoGiantCropProb
                    }),
                    settings.GiantCrops
                    });
                }
                RefValue[] distribution = new RefValue[3]
                {
                    new RefValue(0),
                    new RefValue(0),
                    new RefValue(0),
                };
                IValue[] cropAmounts = new IValue[3]
                {
                    new Expression(new IValue[]
                    {
                        new Term(new IValue[] { qualityCrops, distribution[0] }),
                        normalCrops
                    }),
                    new Term(new IValue[] { qualityCrops, distribution[1] }),
                    new Term(new IValue[] { qualityCrops, distribution[2] })
                };
            }
            if(true) //forage crop
            {
                int numChoices = 4; //cropItems.Length
                IValue[] values = ForageAmounts(numChoices, ForageDistribution);
            }

            Qualities = new Quality[]
            {
                new Quality("Silver", 1.25),
                new Quality("Gold", 1.5),
                new Quality("Iridium", 2)
            };

            Processors = new Processor[4] {
                new Processor("Preserves Jar"),
                new Processor("Keg"),
                new Processor("Oil Maker"),
                new Processor("Mill"),
            };

            SellSources = new Source[6]
            {
                new Processor("Raw Crop"),
                Processors[0],
                Processors[1],
                Processors[2],
                Processors[3],
                new Processor("Seeds")
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
            Dictionary<string, Source> sellSources = SourceDictionary(SellSources);
            Dictionary<string, Source> buySources = SourceDictionary(BuySources);
            Dictionary<string, Source> replantMethods = SourceDictionary(ReplantMethods);
            Dictionary<string, Skill> skillDict = new Dictionary<string, Skill>();
            foreach(Skill skill in Skills.Skills)
            {
                skillDict.Add(skill.Name, skill);
            }

            Crops = new CropDIO[0];
            List<FertilizerDIO> fertilizers = new List<FertilizerDIO>();
            foreach(IConfigurationSection fert in config.GetSection("Fertilizers").GetChildren())
            {
                fertilizers.Add(new FertilizerDIO(
                    fert.GetValue<string>("Name"),
                    fert.GetValue<int>("Quality"),
                    fert.GetValue<double>("Speed"),
                    SerializeBuyPrices(fert, buySources, skillDict, date)));
            }
            Fertilizers = fertilizers.ToArray();
        }

        private static Dictionary<string, Source> SourceDictionary(Source[] sources)
        {
            Dictionary<string, Source> dict = new Dictionary<string, Source>();
            foreach(Source source in sources)
            {
                dict.Add(source.Name, source);
            }
            return dict;
        }

        private static ICondition[] SerializeConditions(
            IConfiguration config,
            Dictionary<string, Skill> skillDict,
            DateState date)
        {
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

        private static BestDict<Source, BuyPrice> SerializeBuyPrices(
            IConfiguration config,
            Dictionary<string, Source> sources,
            Dictionary<string, Skill> skillDict,
            DateState date)
        {
            Dictionary<Source, BuyPrice> prices = new Dictionary<Source, BuyPrice>();
            foreach (IConfigurationSection price in config.GetSection("Sources").GetChildren())
            {
                IConfigurationSection conditions = price.GetSection("Conditions");
                if (conditions.Exists())
                {
                    Source source = sources[price.GetValue<string>("Name")];
                    prices.Add(source, new BuyPrice(
                        price.GetValue<int>("Price"),
                        source,
                        SerializeConditions(conditions, skillDict, date)));
                }
                else
                {
                    Source source = sources[price.GetValue<string>("Name")];
                    prices.Add(source, new BuyPrice(
                        price.GetValue<int>("Price"),
                        source));
                }
            }
            return new BestDict<Source, BuyPrice>(prices);
        }
    }
}
