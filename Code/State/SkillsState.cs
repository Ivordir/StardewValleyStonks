namespace StardewValleyStonks
{
    public class SkillsState
    {

        public Skill[] Skills { get; }

        public Skill Farming { get; }
        public MultiplierProfession Tiller { get; }
        public MultiplierProfession Agriculturist { get; }
        public MultiplierProfession Artisan { get; }

        public Skill Foraging { get; }
        public MultiplierProfession Gatherer { get; }
        public Profession Botanist { get; }

        public SkillsState()
        {
            Skills = new Skill[] { Farming, Foraging };

            Farming = new Skill(
                "Farming",
                new IProfession[][]
                {
                    new IProfession[] { Tiller },
                    new IProfession[] { Agriculturist, Artisan }
                });

            Tiller = new MultiplierProfession(
                "Tiller",
                1.1,
                Farming,
                new ICondition[] { new SkillLvlCondition(Farming, 5) },
                new IProfession[] { Agriculturist, Artisan });

            Artisan = new MultiplierProfession(
                "Artisan", 
                1.4,
                Farming,
                new ICondition[] { new SkillLvlCondition(Farming, 10) },
                null,
                new IProfession[] { Tiller },
                new IProfession[] { Agriculturist });

            Agriculturist = new MultiplierProfession(
                "Agriculturist",
                0.1,
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
                "Gatherer",
                1.2,
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
