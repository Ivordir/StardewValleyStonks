using System.Collections;

namespace StardewValleyStonks
{
    public class Skills : IEnumerable
    {
        public Skill Farming { get; }
        public MultiplierProfession Tiller { get; }
        public MultiplierProfession Agriculturist { get; }
        public MultiplierProfession Artisan { get; }
        public Skill Foraging { get; }
        public MultiplierProfession Gatherer { get; }
        public Profession Botanist { get; }
        public bool IgnoreConflicts { get; set; }

        public Skill this[int index] => _Skills[index];
        IEnumerator IEnumerable.GetEnumerator() => _Skills.GetEnumerator();

        private readonly Skill[] _Skills;

        public Skills()
        {
            Profession[][] farmingProfessions = new Profession[2][];
            Farming = new Skill(
            "Farming",
            farmingProfessions);

            Profession[] tiller = new Profession[2];
            Tiller = new MultiplierProfession(
                "Tiller",
                1.1,
                this,
                new ICondition[] { new SkillLvlCondition(Farming, 5) },
                tiller);

            Profession[] artisanRequirements = new Profession[1];
            Profession[] artisanExclusive = new Profession[1];
            Artisan = new MultiplierProfession(
                "Artisan",
                1.4,
                this,
                new ICondition[] { new SkillLvlCondition(Farming, 10) },
                null,
                artisanRequirements,
                artisanExclusive);

            Profession[] agriculturistRequirements = new Profession[1];
            Profession[] agriculturistExclusive = new Profession[1];
            Agriculturist = new MultiplierProfession(
                "Agriculturist",
                0.1,
                this,
                new ICondition[] { new SkillLvlCondition(Farming, 10) },
                null,
                agriculturistRequirements,
                agriculturistExclusive);

            farmingProfessions[0] = new Profession[] { Tiller };
            farmingProfessions[1] = new Profession[] { Agriculturist, Artisan };
            tiller[0] = Agriculturist;
            tiller[1] = Artisan;
            agriculturistRequirements[0] = Tiller;
            agriculturistExclusive[0] = Artisan;
            artisanRequirements[0] = Tiller;
            artisanExclusive[0] = Agriculturist;

            Profession[][] foraging = new Profession[2][];
            Foraging = new Skill(
                "Foraging",
                foraging);

            Profession[] gatherer = new Profession[1];
            Gatherer = new MultiplierProfession(
                "Gatherer",
                1.2,
                this,
                new ICondition[] { new SkillLvlCondition(Foraging, 5) },
                gatherer);

            Profession[] botanist = new Profession[1];
            Botanist = new Profession(
                "Botanist",
                this,
                new ICondition[] { new SkillLvlCondition(Foraging, 10) },
                null,
                botanist);

            foraging[0] = new Profession[] { Gatherer };
            foraging[1] = new Profession[] { Botanist };
            gatherer[0] = Botanist;
            botanist[0] = Gatherer;

            _Skills = new Skill[] { Farming, Foraging };
        }
    }
}
