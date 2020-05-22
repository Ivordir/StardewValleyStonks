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

        public int GrowthTimeWith(double speed) => Grow.Time(SpeedMultiplier + speed);
        public int HarvestsWithin(int days, double speed = 0) => Grow.HarvestsWithin(days, SpeedMultiplier + speed);
        public int HarvestsWithin(ref int days, double speed = 0) => Grow.HarvestsWithin(ref days, SpeedMultiplier + speed);

        private void ApplyDistribution(double[] dist)
        {
            Inputs[Crops[0]] = dist[0] * QualityCrops + NormalCrops;
            Inputs[Crops[1]] = dist[1] * QualityCrops;
            Inputs[Crops[2]] = dist[2] * QualityCrops;
        }

        public bool DestroysFertilizer { get; }
        private readonly Grow Grow;
        private readonly double SpeedMultiplier;
        private readonly Source BuySource;
        private readonly IItem[] Crops;
        private readonly double QualityCrops;
        private readonly double NormalCrops;
        private readonly Dictionary<IItem, double> Inputs;

        public Crop(CropDIO crop, Source buySource)
        {
            Name = crop.Name;
            Grow = crop.Grow;
            foreach(IMultiplier multiplier in crop.SpeedMultipliers)
            {
                if (multiplier.Active)
                {
                    SpeedMultiplier += multiplier.Value;
                }
            }
            BuySource = buySource; //needs to be a copy from factory
            QualityCrops = crop.QualityCrops;
            NormalCrops = crop.NormalCrops;
        }
    }
}
