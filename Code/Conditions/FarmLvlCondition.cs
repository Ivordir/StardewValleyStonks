using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public class FarmLvlCondition : ICondition
    {
        [Inject] private SkillsState Skills { get; }

        public int FarmLvl { get; }

        public FarmLvlCondition(int farmLvl)
        {
            FarmLvl = farmLvl;
        }

        public bool IsMet => Skills.FarmLvl >= FarmLvl;
        public string WarningMessage => $"Farming level too low. Unlocks at level {FarmLvl}.";
    }
}
