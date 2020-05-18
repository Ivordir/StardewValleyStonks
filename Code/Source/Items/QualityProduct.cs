namespace StardewValleyStonks
{
    public class QualityProduct : IItem
    {
        public string Name => Product.Qualities ? $"{Product.Name} ({Quality.Name})" : Product.Name;
        public int Price => Product.Qualities ? Quality.ApplyTo(Product.Price) : Product.Price;

        private readonly Product Product;
        private readonly Quality Quality;

        public QualityProduct(
            Product product,
            Quality quality)
        {
            Product = product;
            Quality = quality;
        }
    }
}
