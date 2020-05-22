namespace StardewValleyStonks
{
    public class YearCondition : ICondition
    {
        public bool Override { get; set; }
        public bool IsMet => Override || Date.Year >= Year;
        public Warning Warning { get; }

        private readonly Date Date;
        private readonly int Year;

        public YearCondition(Date date, int year)
        {
            Year = year;
            Date = date;
            Warning = new Warning($"Available from year {Year} and onwards.");
        }
    }
}
