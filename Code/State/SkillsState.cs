namespace StardewValleyStonks
{
    public class SkillsState
    {
        public IPriceMultiplier[] Qualities { get; }
        public Skill[] Skills { get; }

        public Skill Farming { get; }
        public PriceProfession Tiller { get; }
        public MultiplierProfession Agriculturist { get; }
        public PriceProfession Artisan { get; }

        public Skill Foraging { get; }
        public MultiplierProfession Gatherer { get; }
        public Profession Botanist { get; }

        public SkillsState()
        {
            Qualities = new IPriceMultiplier[]
            {
                new PriceMultiplier("Silver", 1.25),
                new PriceMultiplier("Gold", 1.5),
                new PriceMultiplier("Iridium", 2)
            };

            Skills = new Skill[] { Farming, Foraging };

            Farming = new Skill(
                "Farming",
                new IProfession[][]
                {
                    new IProfession[] { Tiller },
                    new IProfession[] { Agriculturist, Artisan }
                });

            Tiller = new PriceProfession(
                "Tiller",
                1.1,
                new ICondition[] { new SkillLvlCondition(Farming, 5) },
                new IProfession[] { Agriculturist, Artisan });

            ICondition[] farmLvl10 = new ICondition[] { new SkillLvlCondition(Farming, 10) };

            IProfession[] NeedsTiller = new IProfession[] { Tiller };
            Artisan = new PriceProfession(
                "Artisan",
                1.4,
                farmLvl10,
                null,
                NeedsTiller,
                new IProfession[] { Agriculturist });

            Agriculturist = new MultiplierProfession(
                "Agriculturist",
                0.1,
                farmLvl10,
                null,
                NeedsTiller,
                new IProfession[] { Artisan });


            Foraging = new Skill(
                "Foraging",
                new IProfession[][]
                { 
                    new IProfession[] { Gatherer },
                    new IProfession[] { Botanist } 
                });

            Gatherer = new MultiplierProfession(
                "Gatherer",
                1.2,
                new ICondition[] { new SkillLvlCondition(Foraging, 5) },
                new IProfession[] { Botanist });

            Botanist = new Profession(
                "Botanist",
                new ICondition[] { new SkillLvlCondition(Foraging, 10) },
                null,
                new IProfession[] { Gatherer });
        }
    }
}
