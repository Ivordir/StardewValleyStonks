using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Collections;
using ExtentionsLibrary.Permutations;

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
        private List<CropUsage> Replant()
        {
            List<CropUsage> usages = new List<CropUsage>();
            double seeds = 0;
            Dictionary<Item, QualityDist> inputs = new Dictionary<Item, QualityDist>(InputAmounts);

            //which harvested items cannot be sold/put through a process, but have been indicated to be used for seeds:
            IEnumerable<Item> noCost = Processes.Keys.Where(item => Processes[item].Length == 0);
            double noCostSeeds = 0;
            if (noCost.Any())
            {
                foreach (Item item in noCost)
                {
                    QualityDist quality = inputs[item];
                    while(!quality.Empty)
                    {
                        seeds += Replants[item][quality].OutputConsume(ref quality);
                    }
                }
                inputs.RemoveAll(noCost);
                noCostSeeds = seeds;
            }
            //add noCostSeeds to plans;

            //now go through all items that can be for seeds or can be sold.
            while (seeds < 1)
            {
                List<QualityItem> cheapestReplants = new List<QualityItem>();
                double lowestLoss = double.MaxValue;
                foreach(Item item in inputs.Keys)
                {
                    for (int quality = 0; quality < inputs[item].Length; quality++)
                    {
                        double profitLossPerSeed =
                           Processes[item][quality].Profit(quality)
                           / Replants[item][quality].OutputAmount(quality);
                        if (profitLossPerSeed == lowestLoss)
                        {
                            cheapestReplants.Add(item.With(quality));
                        }
                        else if (profitLossPerSeed < lowestLoss)
                        {
                            cheapestReplants.Clear();
                            cheapestReplants.Add(item.With(quality));
                        }
                    }
                }

                double totalSeedsCanAdd = cheapestReplants.
                    Sum(qi => Replants[qi][qi].OutputAmount(qi));
                if (totalSeedsCanAdd + seeds > 1)
                {
                    foreach (var permutation in ReplantPermutations(cheapestReplants, 1 - seeds))
                    {
                        foreach (QualityItem item in permutation)
                        {
                            //create a cropusage with this item as the last item used for seeds,
                            //so it is not fully used, some will also be sold.
                        }
                    }
                }
                //else if equal
                    //create one usage with all cheapestReplants
                //else 
                    //permute this with next cheapestReplants

                seeds += totalSeedsCanAdd;
            }
            return usages;
        }

        List<IEnumerable<QualityItem>> ReplantPermutations(List<QualityItem> order, double seeds, int index = 0)
        {
            List<IEnumerable<QualityItem>> usages = new List<IEnumerable<QualityItem>>();
            for (int i = index; i < order.Count; i++)
            {
                QualityItem item = order[i];
                double seedsCreated = Replants[item][item].OutputAmount(item);
                if (seedsCreated < seeds)
                {
                    foreach(var permutation in ReplantPermutations(order, seeds - seedsCreated, i + 1))
                    {
                        usages.Add(permutation.Prepend(item));
                    }
                }
                else
                {
                    usages.Add(new QualityItem[] { item });
                }
            }
            //if (usages.Count == 0)
            //{
            //    usages.Add()
            //}
            return usages;
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
