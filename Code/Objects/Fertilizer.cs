namespace StardewValleyStonks
{
    public class Fertilizer
    {
        public string Name { get; }
        public int Quality { get; }
        public double Speed { get; }
        public int Price { get; }
        public Source[] Sources { get; }

        public Fertilizer(FertilizerDIO fert)
        {
            Name = fert.Name;
            Quality = fert.Quality;
            Speed = fert.Speed;
            BuyPrice[] prices = fert.BestPrices.ToArray();
            Price = prices[0].Price;
            Sources = new Source[prices.Length]; 
            for(int i = 0; i < prices.Length; i++)
            {
                Sources[i] = prices[i].Source;
            }
        }
    }
}
