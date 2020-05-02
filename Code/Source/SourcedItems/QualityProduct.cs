using System;
using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public class QualityProduct : IProduct, IComparable<IProduct>
    {
        [Inject] private SkillsState Skills { get; }
        public int Quality { get; }

        private readonly Product Product;

        public QualityProduct(int quality, Product product, bool selected = true)
        {
            Quality = quality;
            Product = product;
            Selected = selected;
        }

        public double Price => OutputPerInput * UnitPrice;

        public string Name => Product.Name;
        public int UnitPrice => (int)(Skills.Quality[Quality] * Product.UnitPrice);
        public double OutputPerInput => Product.OutputPerInput;
        public bool Selected { get; set; }
        public ICondition[] Conditions => Product.Conditions;
        public bool ConditionsMet => Product.ConditionsMet;
        public bool Active => Selected && ConditionsMet;

        public int CompareTo(IProduct other)
        {
            return Price.CompareTo(other.Price);
        }
    }
}
