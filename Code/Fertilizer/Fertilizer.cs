using System;
using ExtentionsLibrary.Memoization;

namespace StardewValleyStonks
{
    public class Fertilizer : INullComparable<Fertilizer>
    {
        readonly static Func<int, int, double[]> CropQualityDist =
           new Func<int, int, double[]>((fertQuality, FarmBuffLevel) =>
           {
               double[] dist = new double[3];
               dist[2] = 0.01 + 0.2 * (FarmBuffLevel / 10.0 + fertQuality * (FarmBuffLevel + 2) / 12.0);
               dist[1] = Math.Min(2 * dist[2], 0.75) * (1 - dist[2]);
               dist[0] = 1 - dist[1] - dist[2];
               return dist;
           }).Memoize();

        public string Name { get; }
        public int Price { get; }
        public Source[] Sources { get; }
        public int Quality { get; }
        public double Speed { get; }
        public double[] Distribution => CropQualityDist(Quality, FarmBuffLevel);

        public int? CompareTo(Fertilizer other)
        {
            bool anyBetter = Price < other.Price || Quality > other.Quality || Speed > other.Speed;
            bool anyWorse = Price > other.Price || Quality < other.Quality || Speed < other.Speed;
            if (anyBetter && anyWorse)
            {
                return null;
            }
            else if (anyBetter)
            {
                return 1;
            }
            else if (anyWorse)
            {
                return -1;
            }
            return 0;
        }

        readonly int FarmBuffLevel;

        public Fertilizer(
            string name,
            int quality,
            double speed,
            int price,
            Source[] sources,
            int farmBuffLevel)
        {
            Name = name;
            Quality = quality;
            Speed = speed;
            Price = price;
            Sources = sources;
            FarmBuffLevel = farmBuffLevel;
        }
    }
}
