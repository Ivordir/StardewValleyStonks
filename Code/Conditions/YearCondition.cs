using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public class YearCondition : ICondition
    {
        [Inject] private DateState Date { get; }

        public int Year { get; }

        public YearCondition(int year)
        {
            Year = year;
        }

        public bool IsMet => Date.Year >= Year;
        public string WarningMessage => $"Available from year {Year} and onwards.";
    }
}
