using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class Crop
    {
        public string Name { get; }
        public Seasons Seasons { get; }
        public int[] GrowthStages => Grow.GrowthStages;
        public bool Regrows => Grow.Regrows;
        public int RegrowTime => Grow.RegrowTime;
        public int Price { get; }
        public Source[] Sources { get; }

        public bool DestroysFertilizer { get; }
        public bool FertilizerCompatable { get; }
        public bool IndoorsOnly { get; }

        readonly Grow Grow;
        readonly double SpeedMultiplier;
        readonly Source BuySource;
        readonly Item CropItem;
        readonly double QualityCrops;
        readonly double NormalCrops;
        readonly Dictionary<Item, QualityDist> InputAmounts;

        public int GrowthTimeWith(double speed) =>
            Grow.DaysPerHarvest(SpeedMultiplier + speed);
        public int HarvestsWithin(int days, double speed = 0) =>
            Grow.HarvestsWithin(days, SpeedMultiplier + speed);
        public int HarvestsWithin(ref int days, double speed = 0) =>
            Grow.HarvestsWithin(ref days, SpeedMultiplier + speed);

        private void ApplyDistribution(double[] dist)
        {
            InputAmounts[CropItem] = new double[]
            {
                dist[0] * QualityCrops + NormalCrops,
                dist[1] * QualityCrops,
                dist[2] * QualityCrops
            };
        }

        public Crop(CropDIO crop, Source buySource)
        {
            Name = crop.Name;
            Grow = crop.Grow;
            SpeedMultiplier = crop.SpeedMultipliers
                .Where(m => m.Active)
                .Sum(m => m.Value);
            BuySource = buySource; // needs to be a copy from factory
            CropItem = crop.Crop;
            QualityCrops = crop.QualityCrops;
            NormalCrops = crop.NormalCrops;
            InputAmounts = crop.HarvestedItems.ToDictionary(
                kvp => kvp.Key,
                kvp => new QualityDist(kvp.Value.Select(v => (double)v).ToArray()));
            Price = crop.Price;
            Sources = crop.BestPrices
                .Select(x => x.Source)
                .ToArray();
        }
    }
}
