namespace StardewValleyStonks
{
    public class QualityProduct : IItem
    {
        public string Name => $"{Product.Name} ({_Quality.Name})";
        public int Price => _Quality.ApplyTo(Product.Price);
        public IItem Normal => Product;

        private readonly Product Product;
        private readonly Quality _Quality;

        public QualityProduct(
            Product product,
            Quality quality)
        {
            Product = product;
            _Quality = quality;
        }
    }
}
