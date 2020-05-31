using System;

namespace StardewValleyStonks
{
    public class Product : IItem
    {
        public string Name => _Name(BaseItem.Name);
        public int Price => Multiplier != null && Multiplier.Active ? (int)(Multiplier.Value * _Price(BaseItem.Price)) : _Price(BaseItem.Price);
        public QualityItem With(int quality) => new QualityItem(ToItem(), quality);
        public QualityItem Normal => new QualityItem(ToItem(), 0);

        readonly Item BaseItem;
        readonly Func<string, string> _Name;
        readonly Func<int, int> _Price;
        readonly IMultiplier Multiplier;

        public Item ToItem() => new Item(Name, Price, Multiplier);

        public Product(
            Item baseItem,
            Func<string, string> nameTransform,
            Func<int, int> priceTransform,
            IMultiplier multiplier = null)
        {
            BaseItem = baseItem;
            _Name = nameTransform;
            _Price = priceTransform;
            Multiplier = multiplier;
        }
    }
}
