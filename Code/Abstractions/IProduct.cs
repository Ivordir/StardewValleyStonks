namespace StardewValleyStonks
{
    public interface IProduct : IPricedItem
    {
        public double Price { get; }

        public double QualityPrice(double quality);
    }
}
