using System;
using System.Collections.Generic;
using System.Linq;
using ExtentionsLibrary.Memoization;

namespace StardewValleyStonks
{
    public class Output
    {
        //StaticCrop[] Crops;
        //StaticRegrowCrop[] RegrowCrops;
        //StaticFertilizer[] Fertilizers;
        //List<PlanNode> Plans;

        private readonly Fertilizer NoFertilizer = new Fertilizer("None", 0, 0, 0);
        private int FarmBuffLevel;
        private Dictionary<Fertilizer, List<Fertilizer>> EqualAlternatesTo;
        private Fertilizer[] Fertilizers;

        public Output()
        {
            EqualAlternatesTo = new Dictionary<Fertilizer, List<Fertilizer>>();
        }

        public void Calculate(Data data, Skills skills)
        {
            FarmBuffLevel = skills.Farming.BuffedLevel;
            EqualAlternatesTo.Clear();

            //make sure NoFert is in list
            Fertilizers = new LinkedList<Fertilizer>(
                data.Fertilizers.Select(f => new Fertilizer(f)))
                .DoComparisons(EqualAlternatesTo)
                .ToArray();
        }

        private static Func<int, int, double[]> QualityDistribution =
            new Func<int, int, double[]>((fertQuality, FarmBuffLevel) =>
            {
                double[] dist = new double[3];
                dist[2] = 0.01 + 0.2 * (FarmBuffLevel / 10.0 + fertQuality * (FarmBuffLevel + 2) / 12.0);
                dist[1] = Math.Min(2 * dist[2], 0.75) * (1 - dist[2]);
                dist[0] = 1 - dist[1] - dist[2];
                return dist;
            }).Memoize();
    }
}
