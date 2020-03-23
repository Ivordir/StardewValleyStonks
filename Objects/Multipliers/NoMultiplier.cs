namespace StardewValleyStonks
{
    public sealed class NoMultiplier : IMultiplier
    {
        public static NoMultiplier Singleton { get; private set; }
        private static readonly double Multiplier;

        static NoMultiplier()
        {
            Multiplier = 1;
            Singleton = new NoMultiplier();
        }

        private NoMultiplier() { }

        public bool Active => false;

        public double Value => Multiplier;
    }
}
