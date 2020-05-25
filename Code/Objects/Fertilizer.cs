namespace StardewValleyStonks
{
    public class Fertilizer : INullComparable<Fertilizer>
    {
        public string Name { get; }
        public int Price { get; }
        public Source[] Sources { get; }
        public int Quality { get; }
        public double Speed { get; }

        public int? CompareTo(Fertilizer other)
        {
            bool anyBetter = Price < other.Price || Quality > other.Quality || Speed > other.Speed;
            bool anyWorse = Price > other.Price || Quality < other.Quality || Speed < other.Speed;
            if (anyBetter && anyWorse)
            {
                return null;
            }
            if (anyBetter)
            {
                return 1;
            }
            else if (anyWorse)
            {
                return -1;
            }
            return 0;
        }

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
