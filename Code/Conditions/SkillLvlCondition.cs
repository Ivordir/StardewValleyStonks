namespace StardewValleyStonks
{
    public class SkillLvlCondition : ICondition
    {
        public bool Override { get; set; }
        public bool IsMet => Override || Skill.Level >= Level;
        public Warning Warning { get; }

        private readonly Skill Skill;
        private readonly int Level;

        public SkillLvlCondition(Skill skill, int level)
        {
            Skill = skill;
            Level = level;
            Warning = new Warning($"{Skill.Name} level too low. Unlocks at level {Level}.");
        }
    }
}
