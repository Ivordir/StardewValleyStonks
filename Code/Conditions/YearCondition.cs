namespace StardewValleyStonks
{
    public class YearCondition : ICondition
    {
        public bool IsMet => Override || Date.Year >= Year;
        public bool Override { get; set; }
        public string WarningMessage => $"Available from year {Year} and onwards.";

        private readonly DateState Date;
        private readonly int Year;

        public YearCondition(DateState date, int year)
        {
            Year = year;
            Date = date;
            Override = false;
        }
    }
}
