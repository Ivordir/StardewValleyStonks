using System.Linq;
using System.Collections.Generic;

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

        public double FertilizerDestroyed { get; } //should always be 0 for regrowCrops
        public bool FertilizerCompatable { get; }
        public bool IndoorsOnly { get; }

        readonly Grow Grow;
        readonly double SpeedMultiplier;

        readonly Item CropItem;
        readonly double QualityCrops;
        readonly double NormalCrops;

        readonly Dictionary<Item, QualityDist> InputAmounts;
        readonly Dictionary<Item, Process[]> Processes;
        readonly Dictionary<Item, Process[]> Replants;
        readonly bool BuySeeds;

        public int GrowthTimeWith(double speed) =>
            Grow.DaysPerHarvest(SpeedMultiplier + speed);
        public int HarvestsWithin(int days, double speed = 0) =>
            Grow.HarvestsWithin(days, SpeedMultiplier + speed);
        public int HarvestsWithin(ref int days, double speed = 0) =>
            Grow.HarvestsWithin(ref days, SpeedMultiplier + speed);
        public double Profit(Fertilizer fert, int harvests)
        {
            Apply(fert.Distribution);
            return Calculate(harvests, Regrows);
        }

        private double Calculate(int harvests, bool Regrows) => 0;
            //also needs processes, replants, inputAmounts, and canBuy
        private double Replant(Process[] Replants, Dictionary<Item, QualityDist> InputAmounts, bool buySeeds, double seeds = 1)
        {
            return 0;
        }
        private double Sold(Process[] processes, Dictionary<Item, QualityDist> inputs)
        {
            return 0;
        }
        private void Apply(double[] dist)
        {
            InputAmounts[CropItem] = new double[]
            {
                dist[0] * QualityCrops + NormalCrops,
                dist[1] * QualityCrops,
                dist[2] * QualityCrops
            };
        }

        public Crop(CropDIO crop, bool buySeeds)
        {
            Name = crop.Name;
            Grow = crop.Grow;
            SpeedMultiplier = crop.SpeedMultipliers
                .Where(m => m.Active)
                .Sum(m => m.Value);
            BuySeeds = buySeeds;
            CropItem = crop.Crop;
            QualityCrops = crop.QualityCrops;
            NormalCrops = crop.NormalCrops;
            InputAmounts = crop.HarvestedItems.ToDictionary(
                kvp => kvp.Key,
                kvp => new QualityDist(kvp.Value.Select(v => v.Value).ToArray()));
            Price = crop.Price;
            Sources = crop.BestPrices.
                Select(x => x.Source).
                ToArray();
        }
    }
}
