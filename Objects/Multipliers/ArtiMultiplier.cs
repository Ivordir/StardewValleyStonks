using Microsoft.AspNetCore.Components;

namespace StardewValleyStonks
{
    public sealed class ArtiMultiplier : IMultiplier
    {
        [Inject] private static SkillsState Skills { get; }

        public static ArtiMultiplier Singleton { get; private set; }

        private static readonly double Multiplier;

        static ArtiMultiplier()
        {
            Multiplier = 1.4; //read from config;
            Singleton = new ArtiMultiplier();
        }

        private ArtiMultiplier() { }

        public bool Active => Skills.Agri;

        public double Value => Multiplier;
    }
}
