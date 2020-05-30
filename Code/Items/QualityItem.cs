using static StardewValleyStonks.Quality;

namespace StardewValleyStonks
{
    public readonly struct QualityItem
    {
        public readonly string Name
        => Quality == 0 
            ? Item.Name
            : $"{Item.Name} ({Get(Quality).Name})";
        public readonly int Price => Get(Quality) * Item.Price;
        public readonly int Quality { get; }
        public readonly Item Item { get; }

        public QualityItem(
            Item item,
            int quality)
        {
            Item = item;
            Quality = quality;
        }
    }
}
