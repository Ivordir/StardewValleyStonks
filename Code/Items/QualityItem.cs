using static StardewValleyStonks.Quality;

namespace StardewValleyStonks
{
    public readonly struct QualityItem : IItem
    {
        public readonly string Name => $"{Normal.Name} ({Get(Quality).Name})";
        public readonly int Price => Get(Quality) * Normal.Price;
        public readonly int Quality { get; }
        public readonly Item Normal { get; }

        public QualityItem(
            Item item,
            int quality)
        {
            Normal = item;
            Quality = quality;
        }
    }
}
