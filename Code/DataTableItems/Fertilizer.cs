using System.Collections.Generic;

namespace StardewValleyStonks
{
    public class Fertilizer : DataTableItem
    {
        public int Quality { get; }
        public double Speed { get; }

        public override bool Active => Selected && PriceManager.HasBestItem && BetterFertilizers.Count == 0;

        public List<Fertilizer> BetterFertilizers
        {
            get
            {
                _BetterFertilizers.Clear();
                foreach(Fertilizer fert in Fertilizers)
                {
                    if (fert.Price <= Price && fert.Quality >= Quality && fert.Speed >= Speed
                        && (fert.Price < Price || fert.Quality > Quality || fert.Speed > Speed))
                    {
                        _BetterFertilizers.Add(fert);
                    }
                }
                return _BetterFertilizers;
            }
        }

        private readonly Fertilizer[] Fertilizers;
        private readonly List<Fertilizer> _BetterFertilizers;

        public Fertilizer(
            string name,
            int quality,
            double speed,
            BestDict<Source, BuyPrice> priceManager,
            Fertilizer[] fertilizers)
            : base(name, priceManager)
        {
            Quality = quality;
            Speed = speed;
            Fertilizers = fertilizers;
            _BetterFertilizers = new List<Fertilizer>();
        }
    }
}
