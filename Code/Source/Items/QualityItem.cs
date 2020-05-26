namespace StardewValleyStonks
{
    public readonly struct QualityItem : IItem
    {
        public string Name => $"{Item.Name} ({Quality.Name})";
        public int Price => Quality * Item.Price;
        public IItem Normal => Item;

        private readonly Item Item;
        private readonly Quality Quality;

        public QualityItem(
            Item item,
            int quality)
        {
            Item = item;
            Quality = Quality.Get(quality);
        }
    }
}
