using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public class FarmLvlCondition : ICondition
    {
        [Inject] private SkillsState Skills { get; }

        public int UnlocksAtLvl { get; }

        public FarmLvlCondition(int unlocksAtLvl)
        {
            UnlocksAtLvl = unlocksAtLvl;
        }

        public bool IsMet => Skills.FarmLvl >= UnlocksAtLvl;
        public string WarningMessage => $"Farming level too low. Unlocks at level {UnlocksAtLvl}.";
    }
}
