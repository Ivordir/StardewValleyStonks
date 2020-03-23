namespace StardewValleyStonks
{
    public interface IMultiplier
    {
        public static IMultiplier Singleton { get; }
        public bool Active { get; }
        public double Value { get; }
    }
}
