namespace StardewValleyStonks
{
    public class QualityItem : IItem
    {
        public string Name => $"{Item.Name} ({Quality.Name})";
        public int Price => Quality.ApplyTo(Item.Price);
        public IItem Normal => Item;

        private readonly Item Item;
        private readonly Quality Quality;

        public QualityItem(
            Item item,
            Quality quality)
        {
            Item = item;
            Quality = quality;
        }
    }
}
