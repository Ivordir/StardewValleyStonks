namespace StardewValleyStonks
{
    public interface IItem
    {
        public string Name { get; }
        public int Price { get; }
        public Item Normal { get; }
        public int Quality { get; }
    }
}
