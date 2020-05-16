namespace StardewValleyStonks
{
    public class YearCondition : ICondition
    {
        private readonly DateState Date;

        public int Year { get; }

        public YearCondition(DateState date, int year)
        {
            Year = year;
            Date = date;
            Override = false;
        }
        public bool Override { get; set; }
        public bool IsMet => Override || Date.Year >= Year;
        public string WarningMessage => $"Available from year {Year} and onwards.";
    }
}
