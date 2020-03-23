using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public sealed class AgriMultiplier : IMultiplier
    {
        [Inject] private static SkillsState Skills { get; }

        public static AgriMultiplier Singleton { get; private set; }

        private static readonly double Multiplier;

        static AgriMultiplier()
        {
            Multiplier = 1.1; //read from config
            Singleton = new AgriMultiplier();
        }

        private AgriMultiplier() { }

        public bool Active => Skills.Agri;

        public double Value => Multiplier;
    }
}
