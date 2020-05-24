﻿using System;
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
        private int FarmBuffLevel;
        private readonly Fertilizer NoFertilizer = new Fertilizer("None", 0, 0, 0);
        private Dictionary<Fertilizer, List<Fertilizer>> EqualAlternatesTo;
        private Fertilizer[] Fertilizers;

        public Output()
        {

        }

        public void Calculate(Data data)
        {
            //make sure NoFert is in list
            LinkedList<Fertilizer> fertilizers = new LinkedList<Fertilizer>
                (data.Fertilizers.Select(f => new Fertilizer(f)));
            EqualAlternatesTo = fertilizers.ToDictionary
                (f => f, f => new List<Fertilizer>());

            fertilizers.DoComparisons(EqualAlternatesTo);

            Fertilizers = fertilizers.ToArray();
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
