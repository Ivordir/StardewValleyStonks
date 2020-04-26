using Microsoft.AspNetCore.Components;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    //for harvestables that have item quality (e.g. crops, forage crops, etc.) 
    public class ProductManager : PriceTracker<IProductSource, IProduct>, IPriceTracker<IProductSource, IProduct>
    {
        [Inject] private SkillsState Skills { get; }

        private readonly int Quality;

        public ProductManager(
            int quality,
            double amount,
            Dictionary<IProductSource, IProduct> productFrom)
            : base(amount, productFrom)
        {
            Quality = quality;
        }

        public override double UnitPrice => NoSource ? 0 : ItemFrom[BestSources[0]].UnitPrice;

        protected override int Compare(IProductSource source)
        {
            return PriceOf(BestSources[0]).CompareTo(PriceOf(source));
        }

        private double PriceOf(IProductSource source)
        {
            return ItemFrom[source].QualityPrice(Skills.Quality[source.HasQuality ? Quality : 0]);
        }
    }
}
