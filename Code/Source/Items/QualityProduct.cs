namespace StardewValleyStonks
{
    public class QualityProduct : IItem
    {
        public string Name => Product.Qualities ? $"{Product.Name} ({Quality.Name})" : Product.Name;
        public int Price => Product.Qualities ? Quality.ApplyTo(Product.Price) : Product.Price;

        private readonly IPriceMultiplier Quality;
        private readonly Product Product;

        public QualityProduct(
            Product product,
            IPriceMultiplier quality)
        {
            Quality = quality;
            Product = product;
        }
    }
}
