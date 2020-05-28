namespace StardewValleyStonks
{
    public readonly struct SkillLvlCondition : ICondition
    {
        public bool IsMet => Skill.Level >= Level;
        public string Warning => $"{Skill.Name} level too low. Unlocks at level {Level}.";

        private readonly Skill Skill;
        private readonly int Level;

        public SkillLvlCondition(Skill skill, int level)
        {
            Skill = skill;
            Level = level;
        }
    }
}
