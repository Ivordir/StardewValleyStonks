using Microsoft.AspNetCore.Components;
using System.Collections.Generic;

namespace StardewValleyStonks
{
    //for harvestables that have item quality (e.g. crops, forage crops, etc.) 
    public class QualityManager : PriceManager<IProductSource, IPricedItem>, IPriceManager<IProductSource, Product>
    {
        [Inject] private SkillsState Skills { get; }

        private readonly int Quality;

        public QualityManager(
            int quality,
            double amount,
            Dictionary<IProductSource, IPricedItem> productFrom)
            : base(amount, productFrom)
        {
            Quality = quality;
        }

        protected override int Compare(IProductSource source)
        {
            return PriceOf(BestSources[0]).CompareTo(PriceOf(source));
        }

        private int PriceOf(IProductSource source)
        {
            return (int)(Skills.Quality[source.HasQuality ? Quality : 0] * ItemFrom[source].Price);
        }
    }
}
