namespace StardewValleyStonks
{
    public class SkillLvlCondition : ICondition
    {
        public bool Override { get; set; }
        public bool IsMet => Override || Skill.Level >= Level;
        public string WarningMessage => $"{Skill.Name} level too low. Unlocks at level {Level}.";

        private readonly Skill Skill;
        private readonly int Level;

        public SkillLvlCondition(Skill skill, int level)
        {
            Skill = skill;
            Level = level;
            Override = false;
        }
    }
}
