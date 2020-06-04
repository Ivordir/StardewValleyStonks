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
        readonly bool BuySeeds;

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

        CropUsage[] Calc()
        {
            List<CropUsage> cropUsages = new List<CropUsage>();
            foreach (var sequence in SecondDraft(new Dictionary<Item, QualityDist>(InputAmounts.Where(kvp => Replants.ContainsKey(kvp.Key)))))
            {
                Dictionary<(Item, Process), Usage> usages = new Dictionary<(Item, Process), Usage>();
                Dictionary<Item, QualityDist> leftOver = new Dictionary<Item, QualityDist>(InputAmounts);
                double seedsToCreate = 1;
                foreach (QualityItem replantItem in sequence.SkipLast(1))
                {
                    QualityDist dist = leftOver[replantItem];
                    double seedsMade = Replants[replantItem][replantItem].OutputConsume(ref dist);
                    var key = (replantItem, Replants[replantItem][replantItem]);
                    if (usages.ContainsKey(key))
                    {
                        usages[key].Inputs[replantItem] = leftOver[replantItem];
                        usages[key].Seeds += seedsMade;
                    }
                    else
                    {
                        double[] inputs = new double[leftOver[replantItem].Length];
                        inputs[replantItem] = leftOver[replantItem];
                        usages.Add(key, new Usage(
                            inputs,
                            Replants[replantItem][replantItem],
                            seedsMade));
                    }
                    seedsToCreate -= seedsMade;
                    if (dist.Empty)
                    {
                        leftOver.Remove(replantItem);
                    }
                    else
                    {
                        leftOver[replantItem] = dist;
                    }
                }
                QualityItem last = sequence.Last();
                QualityDist lastDist = leftOver[last];
                double lastSeedsMade = Replants[last][last].OutputConsume(ref lastDist, seedsToCreate);
                var lastKey = (last, Replants[last][last]);
                if (usages.ContainsKey(lastKey))
                {
                    usages[lastKey].Inputs[last] = leftOver[last];
                    usages[lastKey].Seeds += lastSeedsMade;
                }
                else
                {
                    double[] inputs = new double[leftOver[last].Length];
                    inputs[leftOver[last]] = leftOver[last];
                    usages.Add(lastKey, new Usage(
                        inputs,
                        Replants[last][last],
                        lastSeedsMade));
                }
                seedsToCreate -= lastSeedsMade;
                if (lastDist.Empty)
                {
                    leftOver.Remove(last);
                }
                else
                {
                    leftOver[last] = lastDist;
                }

                foreach (Item item in leftOver.Keys)
                {
                    while (!leftOver[item].Empty)
                    {
                        var key = (item, Processes[item][leftOver[item]]);
                        if (usages.ContainsKey(key))
                        {
                            usages[key].Inputs[leftOver[item]] = leftOver[item];
                        }
                        else
                        {
                            double[] inputs = new double[leftOver[item].Length];
                            inputs[leftOver[item]] = leftOver[item];
                            usages.Add(key, new Usage(
                                inputs,
                                Processes[item][leftOver[item]]));
                        }
                        leftOver[item] = leftOver[item].Next;
                    }
                }
                cropUsages.Add(new CropUsage(
                    seedsToCreate,
                    usages.Values.ToArray()));
            }
            return cropUsages.ToArray();
        }

        List<IEnumerable<QualityItem>> SecondDraft(Dictionary<Item, QualityDist> inputs, double seedsToCreate = 1)
        {
            if (inputs.Count == 0)
            {
                return new List<IEnumerable<QualityItem>> { new QualityItem[0] };
            }
            List<IEnumerable<QualityItem>> usages = new List<IEnumerable<QualityItem>>();

            List<QualityItem> cheapestReplants = new List<QualityItem>();
            double lowestLoss = double.MaxValue;
            foreach (Item item in inputs.Keys)
            {
                int quality = inputs[item];
                double profitLossPerSeed = Processes[item].Length == 0
                    ? 0
                    : Processes[item][quality].Profit(quality)
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

            if (BuySeeds)
            {
                if (lowestLoss < Price)
                {
                    usages.Add(new QualityItem[0]);
                    return usages;
                }
                else if (lowestLoss == Price)
                {
                    usages.Add(new QualityItem[0]);
                }
            }

            double seedsCreated = cheapestReplants.Sum(qi => Replants[qi][qi].OutputAmount(qi));
            if (seedsCreated > seedsToCreate)
            {
                foreach (var permutation in ReplantPermutations(cheapestReplants, seedsToCreate))
                {
                    double maxSeeds = permutation.Sum(qi => Replants[qi][qi].OutputAmount(qi));
                    if (maxSeeds == seedsToCreate) //order doesn't matter, all items are fully consumed.
                    {
                        usages.Add(permutation);
                    }
                    //maxSeeds > seedsToCreate
                    else //order matters: for every item, add one sequence where {item} is used last and therefore not fully consumed.
                    {
                        foreach (QualityItem item in permutation)
                        {
                            if (maxSeeds - Replants[item][item].OutputAmount(inputs[item]) < seedsToCreate)
                            {
                                IEnumerable<QualityItem> e = new QualityItem[] { item };
                                usages.Add(permutation.Except(e).Concat(e));
                            }
                        }
                    }
                    //maxSeeds < seedsToCreate    uhhhhhh error
                }
                return usages;
            }
            else if (seedsCreated == seedsToCreate)
            {
                usages.Add(cheapestReplants);
                return usages;
            }
            foreach (QualityItem item in cheapestReplants)
            {
                inputs[item] = inputs[item].Next;
                if (inputs[item].Empty)
                {
                    inputs.Remove(item);
                }
            }
            foreach (var usage in SecondDraft(inputs, seedsToCreate))
            {
                usages.Add(usage.Concat(cheapestReplants));
            }
            return usages;
        }

        //List<(IEnumerable<QualityItem>, Dictionary<Item, QualityDist>, double)>
        //    ReplantCalc(Dictionary<Item, QualityDist> inputs, double seeds = 0)
        //{
        //    List<(IEnumerable<QualityItem>, Dictionary<Item, QualityDist>, double)> usages
        //        = new List<(IEnumerable<QualityItem>, Dictionary<Item, QualityDist>, double)>();

        //    List<QualityItem> cheapestReplants = new List<QualityItem>();
        //    double lowestLoss = double.MaxValue;
        //    foreach (Item item in inputs.Keys)
        //    {
        //        int quality = inputs[item];
        //        double profitLossPerSeed = Processes[item].Length == 0
        //            ? 0
        //            : Processes[item][quality].Profit(quality)
        //              / Replants[item][quality].OutputAmount(quality);
        //        if (profitLossPerSeed == lowestLoss)
        //        {
        //            cheapestReplants.Add(item.With(quality));
        //        }
        //        else if (profitLossPerSeed < lowestLoss)
        //        {
        //            cheapestReplants.Clear();
        //            cheapestReplants.Add(item.With(quality));
        //        }
        //    }

        //    if (BuySeeds && lowestLoss < Price)
        //    {
        //        usages.Add((new QualityItem[0], inputs, seeds));
        //        return usages;
        //    }

        //    double totalSeedsCanAdd = cheapestReplants.
        //        Sum(qi => Replants[qi][qi].OutputAmount(qi));
        //    if (totalSeedsCanAdd + seeds > 1)
        //    {
        //        foreach (var permutation in ReplantPermutations(cheapestReplants, 1 - seeds))
        //        {
        //            double seedsMade = 0;
        //            Dictionary<Item, QualityDist> amountsToSell = new Dictionary<Item, QualityDist>(inputs);
        //            foreach (QualityItem item in permutation)
        //            {
        //                QualityDist dist = inputs[item];
        //                seedsMade += Replants[item][item].OutputConsume(ref dist);
        //                amountsToSell[item] = dist;
        //            }
        //            foreach (QualityItem item in permutation)
        //            {
        //                double seedsMadeUsingItem = 1 - (seedsMade - Replants[item][item].OutputAmount(inputs[item]));
        //                if (seedsMadeUsingItem > 0)
        //                {
        //                    Dictionary<Item, QualityDist> remain = new Dictionary<Item, QualityDist>(amountsToSell)
        //                    {
        //                        [item] = Replants[item][item].Consume(inputs[item], seedsMadeUsingItem)
        //                    };
        //                    usages.Add((permutation, remain, 0));
        //                }
        //            }
        //        }
        //    }
        //    else
        //    {
        //        foreach (QualityItem item in cheapestReplants)
        //        {
        //            inputs[item] = inputs[item].Next;
        //        }
        //        usages = ReplantCalc(inputs, seeds + totalSeedsCanAdd);
        //        foreach (var usage in usages)
        //        {
        //            usage.Item1.Concat(cheapestReplants);
        //        }
        //    }
        //    if (BuySeeds && lowestLoss == Price)
        //    {
        //        usages.Add((new QualityItem[0], inputs, seeds));
        //    }
        //    return usages;
        //}

        List<IEnumerable<QualityItem>> ReplantPermutations(List<QualityItem> order, double seedsToCreate, int index = 0)
        {
            List<IEnumerable<QualityItem>> usages = new List<IEnumerable<QualityItem>>();
            for (int i = index; i < order.Count; i++)
            {
                QualityItem item = order[i];
                double seedsCreated = Replants[item][item].OutputAmount(item);
                if (seedsCreated < seedsToCreate)
                {
                    foreach (var permutation in ReplantPermutations(order, seedsToCreate - seedsCreated, i + 1))
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
            BuySeeds = buySeeds;
        }
    }

    public class CropUsage
    {
        public CropUsage(double boughtSeeds, Usage[] usages)
        {
            BoughtSeeds = boughtSeeds;
            Usages = usages;
        }

        public double BoughtSeeds { get; }
        public Usage[] Usages { get; }
    }

    public class Usage
    {
        public Usage(double[] inputs, Process process, double seeds = 0)
        {
            Inputs = inputs;
            Process = process;
            Seeds = seeds;
        }

        public double[] Inputs { get; }
        public Process Process { get; }
        public double Seeds { get; set; }
    }
}
