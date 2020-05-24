namespace StardewValleyStonks
{
    public interface IItem
    {
        public string Name { get; }
        public int Price { get; }
        public IItem Normal { get; }
    }
}
