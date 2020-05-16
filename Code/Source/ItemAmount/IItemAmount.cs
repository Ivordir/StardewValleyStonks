namespace StardewValleyStonks
{
    public interface IItemAmount
    {
        public double Amount { get; }
        public IProduct Item { get; }
    }
}