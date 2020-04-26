namespace StardewValleyStonks
{
    public interface IPricedItem : IActiveItem
    {
        public int UnitPrice { get; }
    }
}