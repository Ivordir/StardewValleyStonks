using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class DataState
    {
        public Quality[] Qualities { get; }

        public Crop[] Crops { get; }
        public Fertilizer[] Fertilizers { get; }

        public Source[] SellSources { get; }
        public Processor[] Processors { get; }
        public Source[] BuySources { get; }
        public Source[] ReplantMethods { get; }

        private readonly SelectableMultiplier Irrigated;

        public DataState(SkillsState Skills)
        {
            Qualities = new Quality[]
            {
                new Quality("Silver", 1.25),
                new Quality("Gold", 1.5),
                new Quality("Iridium", 2)
            };

            Irrigated = new SelectableMultiplier("Irrigated", 0.1);

            Crops = new Crop[0];
            Fertilizers = new Fertilizer[0];

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
                new Source("Seed Maker", true, new ICondition[]{ new SkillLvlCondition(Skills.Farming, 9)}),
                new Source("Replant Crop")
            };


            Dictionary<string, IMultiplier> Multipliers = new Dictionary<string, IMultiplier>
            {
                { Skills.Tiller.Name, Skills.Tiller },
                { Skills.Agriculturist.Name, Skills.Agriculturist },
                { Skills.Artisan.Name, Skills.Artisan },
                { Skills.Gatherer.Name, Skills.Gatherer },
                { Irrigated.Name, Irrigated }
            };
            Dictionary<string, Source> sellSources = SourceDictionary(SellSources);
            Dictionary<string, Source> buySources = SourceDictionary(BuySources);
            Dictionary<string, Source> replantMethods = SourceDictionary(ReplantMethods);
        }

        private Dictionary<string, Source> SourceDictionary(Source[] sources)
        {
            Dictionary<string, Source> dict = new Dictionary<string, Source>();
            foreach(Source source in sources)
            {
                dict.Add(source.Name, source);
            }
            return dict;
        }
    }
}
