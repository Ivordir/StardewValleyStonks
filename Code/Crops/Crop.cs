using ExtentionsLibrary.Collections;
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
        readonly Dictionary<QualityItem, Process[]> EqualProcesses;
        readonly Dictionary<QualityItem, Process[]> EqualReplants;

        public int GrowthTimeWith(Fertilizer fert) => GrowthTimeWith(fert.Speed);
        public int GrowthTimeWith(double speed) =>
            Grow.DaysPerHarvest(SpeedMultiplier + speed);
        public int HarvestsWithin(int days, Fertilizer fert) =>
            HarvestsWithin(days, fert.Speed);
        public int HarvestsWithin(int days, double speed = 0) =>
            Grow.HarvestsWithin(days, SpeedMultiplier + speed);
        public int HarvestsWithin(ref int days, Fertilizer fert) =>
            HarvestsWithin(ref days, fert);
        public int HarvestsWithin(ref int days, double speed = 0) =>
            Grow.HarvestsWithin(ref days, SpeedMultiplier + speed);
        public double Profit(Fertilizer fert, int harvests)
        {
            Apply(fert.Distribution);
            return Calculate(harvests, Regrows);
        }

        public bool GrowsIn(Seasons seasons)
        => (Seasons & seasons) > 0;

        private double Calculate(int harvests, bool Regrows) => 0;
            //also needs processes, replants, inputAmounts, and canBuy
        private double Replant()
        {
            List<CropUsage> plans = new List<CropUsage>();
            double seeds = 0;
            Dictionary<Item, QualityDist> inputs = new Dictionary<Item, QualityDist>(InputAmounts);

            //which harvested items cannot be sold/put through a process, but have been indicated to be used for seeds:
            IEnumerable<Item> noCost = Processes.Keys.Where(item => Processes[item].Length == 0);
            double noCostSeeds = 0;
            if (noCost.Any())
            {
                foreach (Item item in noCost)
                {
                    QualityDist itemAmount = inputs[item];
                    while(!itemAmount.Empty)
                    {
                        seeds += Replants[item][itemAmount.Quality].OutputConsume(ref itemAmount);
                    }
                }
                inputs.RemoveAll(noCost);
                noCostSeeds = seeds;
            }

            while (seeds < 1)
            {
                List<Item> cheapestReplants = new List<Item>();
                double lowestLoss = double.MaxValue;
                foreach(Item item in inputs.Keys)
                {
                    int quality = inputs[item].Quality;
                    double profitLossPerSeed =
                        Processes[item][quality].Profit(quality)
                        / Replants[item][quality].OutputAmount(quality);
                    if (profitLossPerSeed == lowestLoss)
                    {
                        cheapestReplants.Add(item);
                    }
                    else if (profitLossPerSeed < lowestLoss)
                    {
                        cheapestReplants.Clear();
                        cheapestReplants.Add(item);
                    }
                }
                double seedsToAdd = cheapestReplants.
                    Sum(i => Replants[i][inputs[i].Quality].OutputAmount(inputs[i].Quality));
                if (seedsToAdd > (1 - seeds))
                {
                    seedsToAdd = 1 - seeds;
                    //order matters.
                    for (int i = 0; i < cheapestReplants.Count; i++)
                    {

                        for (int j = i; j < cheapestReplants.Count; j++)
                        {

                        }
                    }
                }
                else //order doesn't matter
                {

                }


                seeds += seedsToAdd;
            }
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

        public Crop(
            string name,
            Grow grow,
            double speedMultiplier,
            Item cropItem,
            double qualityCrops,
            double normalCrops,
            Dictionary<Item, QualityDist> harvestItems,
            Dictionary<Item, Process[]> processes,
            Dictionary<Item, Process[]> replants,
            Dictionary<QualityItem, Process[]> equalProcesses,
            Dictionary<QualityItem, Process[]> equalReplants,
            bool buySeeds = false,
            int price = -1,
            Source[] sources = null)
        {
            Name = name;
            Grow = grow;
            SpeedMultiplier = speedMultiplier;
            CropItem = cropItem;
            QualityCrops = qualityCrops;
            NormalCrops = normalCrops;
            Price = price;
            Sources = sources;
            InputAmounts = harvestItems;
            Processes = processes;
            Replants = replants;
            EqualProcesses = equalProcesses;
            EqualReplants = equalReplants;
        }
    }

    public class CropUsage
    {
        public IEnumerable<Item> ItemsUsedOnlyForSeeds { get; }
        public double NoCostSeeds { get; }
    }
}
