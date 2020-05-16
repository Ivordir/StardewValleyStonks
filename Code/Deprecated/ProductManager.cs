using Microsoft.AspNetCore.Components;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    //for harvestables that have item quality (e.g. crops, forage crops, etc.) 
    public class ProductManager : PriceTracker<IProcessor, IProduct>, IPriceTracker<IProcessor, IProduct>
    {
        [Inject] private SkillsState Skills { get; }

        private readonly int Quality;

        public ProductManager(
            int quality,
            double amount,
            Dictionary<IProcessor, IProduct> productFrom)
            : base(amount, productFrom)
        {
            Quality = quality;
        }

        protected override int Compare(IProcessor source)
        {
            return PriceOf(BestSources[0]).CompareTo(PriceOf(source));
        }

        private double PriceOf(IProcessor source)
        {
            return ItemFrom[source].QualityPrice(Skills.Quality[source.PreservesQuality ? Quality : 0]);
        }
    }
}
