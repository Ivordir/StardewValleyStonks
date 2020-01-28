using System.Collections.Generic;

namespace StardewValleyBestCropPlanFinder.Client
{
    public class Fertilizer
    {
        public string Name { get; }
        public int Quality { get; }
        public float Speed { get; }
        public Dictionary<Sources, int> Sources { get; }

        public Fertilizer(string name, int quality, float speed, Dictionary<Sources, int> sources)
        {
            Name = name;
            Quality = quality;
            Speed = speed;
            Sources = sources;
        }
    }

}
