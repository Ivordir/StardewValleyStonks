namespace StardewValleyStonks
{
    public class FertilizerDIO : DataTableItem
    {
        public int Quality { get; }
        public double Speed { get; }

        public override bool Active => Selected && PriceManager.HasBestItem;// && BetterFertilizers.Count == 0;

        //public List<FertilizerDIO> BetterFertilizers
        //{
        //    get
        //    {
        //        _BetterFertilizers.Clear();
        //        foreach(FertilizerDIO fert in Fertilizers)
        //        {
        //            if (fert.Price <= Price && fert.Quality >= Quality && fert.Speed >= Speed
        //                && (fert.Price < Price || fert.Quality > Quality || fert.Speed > Speed))
        //            {
        //                _BetterFertilizers.Add(fert);
        //            }
        //        }
        //        return _BetterFertilizers;
        //    }
        //}

        //private readonly FertilizerDIO[] Fertilizers;
        //private readonly List<FertilizerDIO> _BetterFertilizers;

        public FertilizerDIO(
            string name,
            int quality,
            double speed,
            BestDict<Source, BuyPrice> priceManager)//,
            //FertilizerDIO[] fertilizers)
            : base(name, priceManager)
        {
            Quality = quality;
            Speed = speed;
            //Fertilizers = fertilizers;
            //_BetterFertilizers = new List<FertilizerDIO>();
        }
    }
}
