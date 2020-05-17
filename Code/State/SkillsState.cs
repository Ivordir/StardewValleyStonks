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
                new PriceMultiplier("Tiller", 1.1),
                Farming,
                new ICondition[] { new SkillLvlCondition(Farming, 5) },
                new IProfession[] { Agriculturist, Artisan });

            Artisan = new PriceProfession(
                new PriceMultiplier("Artisan", 1.4),
                Farming,
                new ICondition[] { new SkillLvlCondition(Farming, 10) },
                null,
                new IProfession[] { Tiller },
                new IProfession[] { Agriculturist });

            Agriculturist = new MultiplierProfession(
                new Multiplier("Agriculturist", 0.1),
                0,
                Farming,
                new ICondition[] { new SkillLvlCondition(Farming, 10) },
                null,
                new IProfession[] { Tiller },
                new IProfession[] { Artisan });


            Foraging = new Skill(
                "Foraging",
                new IProfession[][]
                {
                    new IProfession[] { Gatherer },
                    new IProfession[] { Botanist }
                });

            Gatherer = new MultiplierProfession(
                new Multiplier("Gatherer", 1.2),
                1,
                Foraging,
                new ICondition[] { new SkillLvlCondition(Foraging, 5) },
                new IProfession[] { Botanist });

            Botanist = new Profession(
                "Botanist",
                Foraging,
                new ICondition[] { new SkillLvlCondition(Foraging, 10) },
                null,
                new IProfession[] { Gatherer });
        }
    }
}
