using System.Collections.Generic;

namespace StardewValleyBestCropPlanFinder.Client
{
    public class Fertilizer
    {
        public string Name { get; }
        public int Quality { get; }
        public float Speed { get; }
        public Dictionary<Sources, int> PriceFrom { get; }

        public Fertilizer(string name, int quality, float speed, Dictionary<Sources, int> priceFrom)
        {
            Name = name;
            Quality = quality;
            Speed = speed;
            PriceFrom = priceFrom;
        }
    }

}
