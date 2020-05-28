using static StardewValleyStonks.Quality;

namespace StardewValleyStonks
{
    public readonly struct QualityItem : IItem
    {
        public readonly string Name => $"{Item.Name} ({_Quality.Name})";
        public readonly int Price => _Quality * Item.Price;
        public readonly int Quality { get; }
        public readonly Item Normal => Item;

        readonly Item Item;
        readonly Quality _Quality => Get(Quality);

        public QualityItem(
            Item item,
            int quality)
        {
            Item = item;
            Quality = quality;
        }
    }
}
