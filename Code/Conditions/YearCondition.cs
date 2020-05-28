namespace StardewValleyStonks
{
    public readonly struct YearCondition : ICondition
    {
        public readonly bool IsMet => Date.Year >= Year;
        public readonly string Warning => $"Available from year {Year} and onwards.";

        readonly Date Date;
        readonly int Year;

        public YearCondition(Date date, int year)
        {
            Year = year;
            Date = date;
        }
    }
}
