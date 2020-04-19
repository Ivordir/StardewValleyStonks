using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public class YearCondition : ICondition
    {
        [Inject] private DateState Date { get; }

        public int UnlocksAtYear { get; }

        public YearCondition(int unlocksAtYear)
        {
            UnlocksAtYear = unlocksAtYear;
        }

        public bool IsMet => Date.Year >= UnlocksAtYear;
        public string WarningMessage => $"Available from year {UnlocksAtYear} and onwards.";
    }
}
