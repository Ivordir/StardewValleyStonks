namespace StardewValleyStonks
{
    public class Fertilizer : ICanCompare<Fertilizer>
    {
        public string Name { get; }
        public int Price { get; }
        public Source[] Sources { get; }
        public int Quality { get; }
        public double Speed { get; }

        public bool CanCompareTo(Fertilizer other)
        {
            bool firstComparison = Price <= other.Price;
            return (Quality >= other.Quality) == firstComparison
                && (Speed >= other.Speed) == firstComparison;
        }
        public int CompareTo(Fertilizer other) => Price.CompareTo(other.Price);

        public Fertilizer(FertilizerDIO fert)
        {
            Name = fert.Name;
            Quality = fert.Quality;
            Speed = fert.Speed;
            Price[] prices = fert.BestPrices.ToArray();
            Price = prices[0].Value;
            Sources = new Source[prices.Length]; 
            for(int i = 0; i < prices.Length; i++)
            {
                Sources[i] = prices[i].Source;
            }
        }

        public Fertilizer(
            string name,
            int quality,
            double speed,
            int price,
            Source[] sources = null)
        {
            Name = name;
            Quality = quality;
            Speed = speed;
            Price = price;
            Sources = sources ?? NoSource;
        }

        private static readonly Source[] NoSource = new Source[0];
    }
}
