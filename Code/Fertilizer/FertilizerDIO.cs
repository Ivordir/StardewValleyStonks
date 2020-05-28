using System;
using System.Collections.Generic;
using System.Linq;

namespace StardewValleyStonks
{
    public class FertilizerDIO : DataTableItem
    {
        public int Quality { get; }
        public double Speed { get; }
        public override bool Active => Selected && HasPrice;
        public override string Warnings
        {
            get
            {
                string warnings = string.Empty;
                if (!HasPrice)
                {
                    string priceWarnings = string.Empty;
                    foreach (Price price in Prices)
                    {
                        if (!price.Active)
                        {
                            priceWarnings += price.Warnings.WrapTag("li");
                        }
                    }
                    warnings += $"{ Name } cannot be bought from anywhere:" + priceWarnings.WrapTag("ul");
                }
                return warnings;
            }
        }

        public Fertilizer ToFertilizer(int farmBuffLevel) => new Fertilizer(
            Name,
            Quality,
            Speed,
            Price,
            BestPrices.Select(p => p.Source).ToArray(),
            farmBuffLevel);

        public FertilizerDIO(
            string name,
            int quality,
            double speed,
            Dictionary<Source, Price> priceFrom)
            : base(name, priceFrom)
        {
            Quality = quality;
            Speed = speed;
        }
    }
}
