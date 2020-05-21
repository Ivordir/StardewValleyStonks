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

        private readonly Grow Grow;
        private readonly double SpeedMultiplier;
        private readonly Source BuySource;
        private readonly RefValue[] Distribution;

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
        }
    }
}
