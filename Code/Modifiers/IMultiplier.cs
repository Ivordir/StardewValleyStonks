namespace StardewValleyStonks
{
    public interface IMultiplier : ISelectable
    {
        public string Name { get; }
        public double Value { get; }

        public void Save();
    }
}
