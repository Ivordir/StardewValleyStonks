namespace StardewValleyBestCropPlanFinder.Client
{
    public class Fertilizer
    {
        public string Name { get; }
        public int Quality { get; }
        public float Speed { get; }
        public int Price { get; }

        public Fertilizer(string name, int quality, float speedMultiplier, int price)
        {
            Name = name;
            Quality = quality;
            Speed = speedMultiplier;
            Price = price;
        }
    }

}
